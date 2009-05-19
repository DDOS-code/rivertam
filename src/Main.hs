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

import Control.Exception hiding (try)
import qualified Data.Map as M

import Command2
import CommandInterface
import Helpers
import IRC
import Parse
import Send
import Config
import Paths

type BracketBundle = (Handle, TChan String, Config, FilePath, FilePath, IrcState)

main :: IO ()
main = withSocketsDo $ bracket initialize finalize mainloop

initialize :: IO BracketBundle
initialize = do
	configPath	<- getConfigPath "rivertam/"
	dataPath	<- getDataPath
	config		<- getConfigIO (configPath, "river.conf")
	mapM_ putStrLn	[ "!!! Config Path: "++configPath
			, "!!! Data Path: "++dataPath		]

	sock		<- connectTo (network config) (PortNumber (port config))
	hSetBuffering sock NoBuffering

	tchan	 	<- atomically newTChan
	forkIO $ senderThread sock tchan

	forkIO $ forever $ (atomically . writeTChan tchan) =<<  getLine

	return (sock, tchan, config, configPath, dataPath, IrcState {
		  ircNick	= ""
		, ircMap	= M.empty
		})

finalize :: BracketBundle -> IO ()
finalize (sock, _, _, _, _,_) = hClose sock

mainloop :: BracketBundle -> IO ()
mainloop (sock, tchan, config_, configPath, dataPath, state_) = do
	sendMsgs tchan $ evalState (initIRC config_) state_
	initcommands <- initComState configPath dataPath

	loop state_ initcommands config_ where
	loop state comstate config = do
		response <- timeout (20000*1000) $ try $ dropWhileRev isSpace `liftM` hGetLine sock
		case response of
			Nothing	-> do
				newconf	<- getConfig `liftM` readFileStrict (configPath++"river.conf")
				case newconf of
					Left e	-> putStrLn $ "!!! Error in config file: " ++ e
					Right r	-> do
						let (ircMsgs, state') = runState (updateConfig r) state
						sendMsgs tchan ircMsgs
						print $ ircMsgs
						loop state' comstate r
			Just (Left _)	-> return ()
			Just (Right line) -> do
				when (debug config >= 1) $
					putStrLn $ "\x1B[32;1m>>\x1B[30;0m " ++ show line
				case ircToMessage line of
					Nothing -> loop state comstate config
					Just a -> do
						let	((ircMsgs, external), state') = runState (parseMode config a) state
						sendMsgs tchan ircMsgs
						comstate' <- maybe (return comstate) (sendExternal comstate state' config tchan configPath) external
						loop state' comstate' config

sender :: SenderChan -> Response -> IO ()
sender tchan = atomically . writeTChan tchan . responseToIrc

sendMsgs :: SenderChan -> [Response] -> IO ()
sendMsgs tchan = mapM_ (sender tchan)

sendExternal :: ComState -> IrcState -> Config -> SenderChan -> FilePath -> External -> IO ComState
sendExternal state irc config tchan confDir (ExecCommand access chan person string) = command info newstate access person string  where
		newstate = state {
			  echoFunc	= sender tchan . comToIrc
			}
		info = Info {
			  echoFunc2	= sender tchan . comToIrc
			, filePath	= confDir
			, config2	= config
			, myNick	= ircNick irc
			, userList	= maybe [] M.keys $ M.lookup (map toLower chan) (ircMap irc)
			}
		comToIrc dt = case dt of
			Mess a		-> Msg chan a
			Private a	-> Notice person a
