{-
	Rivertam, written by Christoffer Öjeling aka "Cadynum".

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU Affero General Public License as
	published by the Free Software Foundation, either version 3 of the License,
	or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU Affero General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Main where
import Network
import System.IO
import System.Timeout
import System.Directory
import Control.Exception

import CommandInterface
import IRC
import IrcState
import Parse
import Send
import Config

import Database.HDBC
import Database.HDBC.PostgreSQL
import Command
import ComMemos (fetchMemos)

main :: IO ()
main = withSocketsDo $ bracket (initialize initialCState) finalize (runRiver mainloop) >> return ()

initialize :: x -> IO (RState x)
initialize com = do
	configPath	<- getConfigPath "rivertam/"
	putStrLn $ "!!! Config Path: " ++ show configPath
	let riverconf	= configPath ++ "river.conf"

	config_		<- getConfig <$> readFile riverconf
	config 		<- either (\e -> error $ "river.conf: " ++ e) return config_
	configTime	<- getModificationTime riverconf

	sock		<- connectTo (network config) (PortNumber (port config))
	hSetBuffering sock NoBuffering

	sendchan 	<- atomically newTChan

	forkIO $ senderThread sock sendchan
	forkIO $ forever $ (atomically . writeTChan sendchan) =<<  getLine

	conn		<- connectPostgreSQL (pgconn config)

	initTime	<- getUnixTime

	return RState {sock, sendchan, conn, initTime, config, configPath, configTime, ircState=ircInitial, com}


finalize :: (RState x) -> IO ()
finalize RState{sock, sendchan, conn, initTime} = do
	disconnect conn
	atomically $ clearSender sendchan
	uptime <- (-) <$> getUnixTime <*> pure initTime
	sendIrc $ Quit $
		"rivertam - The haskell IRC-bot with style! - Running " ++ formatTime uptime
	threadDelay 1000000 --Give it one second to send the Quit message
	hClose sock
	where sendIrc = atomically . writeTChan sendchan . responseToIrc

mainloop :: River CState ()
mainloop = do
	sendM =<< initIRC <$> gets config
	commandInit
	loop =<< io getMicroTime
	where loop reparseT = do
		now 		<- io getMicroTime
		sock		<- gets sock
		rtime		<- gets (reparsetime . config)

		response	<- let wait = max 0 (rtime - fromInteger (now-reparseT))
					in io $ timeout wait $ hGetLine sock

		updateConfigR
		case dropWhileRev isSpace `liftM` response of
			Nothing	-> do
				sendM =<< updateConfig <$> gets config <*> gets ircState
				loop now
			Just line -> do
				echo $ "\x1B[32;1m>>\x1B[30;0m " ++ show line
				case ircToMessage line of
					Nothing -> loop reparseT
					Just a -> do
						modify $ \x -> x {ircState = ircUpdate a (ircState x)}
						(ircMsgs, external) <- parse <$> gets config <*> gets ircState <*> pure a
						sendM ircMsgs
						mapM_ sendExternal external
						loop reparseT

updateConfigR :: (MonadState (RState x) m, MonadIO m, Functor m) => m ()
updateConfigR = do
	old	<- gets configTime
	path	<- (++"river.conf") <$> gets configPath
	now	<- io $ getModificationTime path
	when (now > old) $ do
		newconf	<- getConfig <$> io (readFile path)
		case newconf of
			Left e	-> trace $ "!!! Error in config file: " ++ e
			Right new -> modify $ \x -> x {configTime=now, config=new}

sendExternal :: External -> River CState ()
sendExternal (ExecCommand access chan nick domain args) = command chan access nick domain args
sendExternal (BecomeActive person) = sendM <$> fmap (Msg person . show) =<< fetchMemos person


getConfigPath :: FilePath -> IO FilePath
getConfigPath name = do
	path 	<- getAppUserDataDirectory name
	t	<- doesDirectoryExist path
	return $! if t then path else ""
