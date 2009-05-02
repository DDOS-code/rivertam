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
import qualified Data.Map as M
import System.IO.Error (try)
import Control.Exception hiding (try)
import System.Directory
import System.Environment
import System.Posix.Signals

import Helpers
import Parse
import Send
import Config
import RiverState
import GeoIP
import Paths_rivertam

import ComTremRelay
mastersrv, masterport :: String
mastersrv =  "master.tremulous.net"
masterport = "30710"
main :: IO ((), River)
main = withSocketsDo $ bracket initialize finalize mainloop where
	initialize = do
		rivConfDir	<- getConfDir
		config		<- getConfigIO (rivConfDir, "river.conf")
		rivSocket	<- connectTo (network config) (PortNumber (port config))
		hSetBuffering rivSocket NoBuffering

		rivUptime	<- getMicroTime

		rivSender 	<- atomically newTChan
		forkIO $ senderThread rivSocket rivSender

		stdinT		<- forkIO $ forever $ (atomically . writeTChan rivSender) =<<  getLine

		rivGeoIP	<- GeoIP.fromFile =<< getDataFileName "IpToCountry.csv"

		rivPhost	<- getDNS mastersrv masterport

		rivTremded	<- initRelay config rivSender
		putStrLn $ "irc->trem relay active: " ++ (show . isJust . fst $ rivTremded)
		putStrLn $ "trem->irc relay active: " ++ (show . isJust . snd $ rivTremded)

		let bSig x	= installHandler x (Catch (sigINTHandler rivSocket rivSender [stdinT])) Nothing
		mapM_ bSig [sigINT, sigTERM, sigABRT, sigQUIT]

		return $! River {
			  rivSender
			, rivSocket
			, rivConfDir
			, config
			, rivNick	= initNick_
			, rivMap	= M.empty
			, rivUptime
			, rivGeoIP
			, rivPoll	= PollNone
			, rivPhost
			, rivTremded
			}

	finalize state = do
		hClose $ rivSocket state
		exitRelay $ rivTremded state
		putStrLn "Clean exit."

	mainloop = runStateT $ do
		Config {name, user, nick} <- gets config
		--Send user & nick info
		Raw >>> "USER " ++ user ++ " 0 * :" ++ name
		Nick >>> nick
		loop where
		loop = do
			River {rivSocket, config} <- get
			response <- lift $ try $ dropWhileRev isSpace `liftM` hGetLine rivSocket
			case response of
				Left _ ->
					return ()
				Right line -> do
					when (debug config >= 1) $
						lift . putStrLn $ "\x1B[32;1m>>\x1B[30;0m " ++ show line
					parseIrcLine line
					loop

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


sigINTHandler :: Handle -> TChan String -> [ThreadId] -> IO ()
sigINTHandler hdl chan threads = do
	atomically $ clearSender chan >> writeTChan chan "QUIT :termination signal received"
	mapM_ killThread threads
	threadDelay 500000 --Lets give the quit message 500ms to be sent.
	hClose hdl
