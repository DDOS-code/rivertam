module RiverState (
	Status(..)
	, River(..)
	, RiverState
	, From
	, Command
	, CommandList
	, CommandInfo
	, module System.IO
	, module Control.Monad.State
) where
import System.IO
import Control.Monad.State
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Network.Socket

import Config
import Helpers
import qualified TremMasterCache
import qualified GeoIP

data Status = Normal | Voice | OP deriving (Show, Eq, Ord)

data River = River
	{ rivSender	:: TChan String
	, rivSocket	:: Handle
	, rivConfDir	:: FilePath
	, config	:: !Config
	, rivMap	:: Map String (Map String Status)
	, rivUptime	:: Integer
	, rivGeoIP	:: GeoIP.Data
	, rivCache	:: TremMasterCache.ServerCache
	, rivCacheTime	:: Integer
	, rivTremdedSock:: (Socket, ThreadId)
	}

type RiverState = StateT River IO ()

type From = ((String, String, String), String, String)
type Command = (String, String, String) -> RiverState

-- (name, (function, min arg required, min access level required, arguments, help info))
type CommandList = [(String, CommandInfo)]
type CommandInfo = (Command, Int, Access, String, String)


