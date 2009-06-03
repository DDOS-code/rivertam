{-
	River-Tam, written by Christoffer Öjeling aka "Cadynum".

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Main where
import Network
import System.IO
import System.IO.Error (try)
import System.Timeout
import System.Directory
import System.Time
import Control.Exception hiding (try)
import Control.Monad.State
import Control.Strategies.DeepSeq
import qualified Data.Map as M

import Command
import CommandInterface
import Helpers
import IRC
import Parse
import Send
import Config
import Paths

type BracketBundle = (Handle, TChan String, Config, FilePath, FilePath, ClockTime, IrcState)

main :: IO ()
main = withSocketsDo $ bracket initialize finalize mainloop

initialize :: IO BracketBundle
initialize = do
	configPath	<- getConfigPath "rivertam/"
	dataPath	<- getDataPath
	config		<- getConfigIO (configPath, "river.conf")
	configTime	<- getModificationTime (configPath++"river.conf")
	mapM_ putStrLn	[ "!!! Config Path: "++configPath
			, "!!! Data Path: "++dataPath		]

	sock		<- connectTo (network config) (PortNumber (port config))
	hSetBuffering sock NoBuffering

	tchan	 	<- atomically newTChan
	forkIO $ senderThread sock tchan

	forkIO $ forever $ (atomically . writeTChan tchan) =<<  getLine

	return (sock, tchan, config, configPath, dataPath, configTime, IrcState {
		  ircNick	= ""
		, ircMap	= M.empty
		})

finalize :: BracketBundle -> IO ()
finalize (sock, _, _, _, _,_,_) = hClose sock

mainloop :: BracketBundle -> IO ()
mainloop (sock, tchan, config_, configPath, dataPath, configTime_, state_) = do
	mapM_ (sender tchan) $ evalState (initIRC config_) state_
	commandState <- initComState configPath dataPath

	let loop state (config, configTime) = do
		response	<- timeout (reparsetime config) $ try $ dropWhileRev isSpace `liftM` hGetLine sock

		configTime'	<- getModificationTime (configPath++"river.conf")
		config'		<- if configTime' <= configTime then return config else do
			newconf	<- getConfig `liftM` readFileStrict (configPath++"river.conf")
			case newconf of
				Left e -> do
					putStrLn $ "!!! Error in config file: " ++ e
					return config
				Right new -> return new

		case response of
			Nothing	-> do
				let (ircMsgs, state') = runState (updateConfig config') state
				mapM_ (sender tchan) ircMsgs
				loop state' (config', configTime')
			Just (Left _)	-> return ()
			Just (Right line) -> do
				when (debug config' >= 1) $
					putStrLn $ "\x1B[32;1m>>\x1B[30;0m " ++ show line
				case ircToMessage line of
					Nothing -> loop state (config', configTime')
					Just a -> do
						let ((ircMsgs, external), state') = runState (parseMode config' a) state
						mapM_ (sender tchan) ircMsgs
						maybe (return ()) (sendExternal commandState state' config' tchan configPath) external
						loop state' (config', configTime')
	loop state_ (config_, configTime_)


sender :: SenderChan -> Response -> IO ()
sender tchan = atomically . writeTChan tchan . responseToIrc

sendExternal :: ComState -> IrcState -> Config -> SenderChan -> FilePath -> External -> IO ()
sendExternal state irc config tchan confDir (ExecCommand access chan person string) = command info state access person string
	where
	info = Info {
		  echo		= sender tchan . Msg chan . strict
		, echop		= sender tchan . Notice person . strict
		, filePath	= confDir
		, config
		, myNick	= ircNick irc
		, userList	= maybe M.empty (M.map (const ())) $ M.lookup (map toLower chan) (ircMap irc)
		}
