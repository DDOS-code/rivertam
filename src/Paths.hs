module Paths where
import System.Directory
import System.IO.Error (try)
import System.Environment
import Control.Monad
import Config
import Helpers
import Paths_rivertam

-- Let's try to get the config file path. First try with XDG, and if that fails, let haskell decide.
-- If the directory doesn't exist, create it.
getConfigPath :: FilePath -> IO FilePath
getConfigPath name = do
	xdg <- try $ getEnv "XDG_CONFIG_HOME"
	path <- case xdg of
		Right fp | not $ null fp ->
			return $ fp ++ "/" ++ name
		_ ->
			getAppUserDataDirectory name

	createDirectoryIfMissing True path
	return $! path


getDataPath :: IO FilePath
getDataPath = getDataFileName ""
--getDataPath = return $ "/home/ojeling/share/rivertam-0.0/"


--Get the config, if the file doesnt exist move the example from the datadir and terminate
getConfigIO :: (FilePath, FilePath) -> IO Config
getConfigIO (pp, fp) = do
	let file	= pp++fp
	conttest	<- try $ readFileStrict file
	datadir		<- (fp++) `liftM` getDataPath
	case conttest of
		Left _ -> do
			putStrLn $ file ++ ": Not found"
			putStrLn $ "Moving example config " ++ datadir ++ " to " ++ pp
			copyFile datadir file
			error "The bot will now terminate, modify the config to your needs and restart it."
		Right c -> return $! (either (\x -> error $ file ++ ": " ++ x) id . getConfig) c
