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
import System.Directory
import System.Environment
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
--import Paths_rivertam
getDataFileName :: FilePath -> IO FilePath
getDataFileName x = return $ "/home/ojeling/share/rivertam-0.0/" ++ x


main :: IO ()
main = withSocketsDo $ bracket initialize finalize mainloop where
	initialize = do
		confDir		<- getConfDir
		config		<- getConfigIO (confDir, "river.conf")

		sock		<- connectTo (network config) (PortNumber (port config))
		hSetBuffering sock NoBuffering
		tchan	 	<- atomically newTChan
		forkIO $ senderThread sock tchan

		forkIO $ forever $ (atomically . writeTChan tchan) =<<  getLine

		return (sock, tchan, confDir, IrcState {
			  config
			, ircNick	= ""
			, ircMap	= M.empty
			})

	finalize (sock, _, _, _) = hClose sock


	mainloop (sock, tchan, confDir, state_) = do
		sendMsgs tchan $ evalState initIRC state_
		initcommands <- initComState confDir

		loop state_ initcommands where
		loop state comstate = do
			response <- timeout (20000*1000) $ try $ dropWhileRev isSpace `liftM` hGetLine sock
			case response of
				Nothing	-> do
					newconf	<- getConfig `liftM` readFileStrict (confDir++"river.conf")
					case newconf of
						Left e	-> putStrLn $ "!!! Error in config file: " ++ e
						Right r	-> do
							let (ircMsgs, state') = runState updateConfig (state {config=r})
							sendMsgs tchan ircMsgs
							print $ ircMsgs
							loop state' comstate
				Just (Left _)	-> return ()
				Just (Right line) -> do
					when ((debug . config $ state) >= 1) $
						putStrLn $ "\x1B[32;1m>>\x1B[30;0m " ++ show line
					case ircToMessage line of
						Nothing -> loop state comstate
						Just a -> do
							let	((ircMsgs, external), state') = runState (parseMode a) state
							print $ ircMsgs
							sendMsgs tchan ircMsgs
							comstate' <- maybe (return comstate) (sendExternal comstate state' tchan) external
							loop state' comstate'

sender :: TChan String -> Response -> IO ()
sender tchan = atomically . writeTChan tchan . responseToIrc

sendMsgs :: TChan String -> [Response] -> IO ()
sendMsgs tchan = mapM_ (sender tchan)

sendExternal :: ComState -> IrcState -> TChan String -> External -> IO ComState
sendExternal state irc tchan (ExecCommand access chan person string) = command newstate access person string  where
		newstate = state {
			  echoFunc	= sender tchan . comToIrc
			, myNick	= ircNick irc
			, userList	= maybe [] M.keys $ M.lookup (map toLower chan) (ircMap irc)
			, conf 		= config irc
			}
		comToIrc dt = case dt of
			Mess a		-> Msg chan a
			Private a	-> Notice person a


--Get the config, if the file doesnt exist move the example from the datadir and terminate
getConfigIO :: (FilePath, FilePath) -> IO Config
getConfigIO (pp, fp) = do
	let file	= pp++fp
	conttest	<- try $ readFileStrict file
	datadir		<- getDataFileName fp
	case conttest of
		Left _ -> do
			putStrLn $ file ++ ": Not found"
			putStrLn $ "Moving example config " ++ datadir ++ " to " ++ pp
			copyFile datadir file
			error "The bot will now terminate, modify the config to your needs and restart it."
		Right c -> return $! (either (\x -> error $ file ++ ": " ++ x) id . getConfig) c

--Get the config path. first try with XDG, then HOME and if that's not found go with relative path
getConfDir :: IO FilePath
getConfDir = do
	cpath <- foo
	putStrLn =<< ("!!! Data part:   "++) `liftM` getDataFileName ""
	putStrLn $ "!!! Config path: " ++ cpath
	createDirectoryIfMissing True cpath

	return cpath
	--Staircasing FTW
	where foo = do
		xdg <- try $ getEnv "XDG_CONFIG_HOME"
		case xdg of
			Right fp | not $ null fp ->
				return $ fp ++ "/rivertam/"
			_ -> do
				home <- try $ getAppUserDataDirectory "rivertam/"
				case home of
					Right fp | not $ null fp  ->
						return fp
					_ -> return "rivertam/"
