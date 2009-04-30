module RiverState (
	  module System.IO
	, module Control.Monad.State
	, Status(..)
	, River(..)
	, RiverState
	, From
	, Command
	, CommandList
	, CommandInfo
	, initNick_
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
	{ rivSender	:: !(TChan String)
	, rivSocket	:: !Handle
	, rivConfDir	:: !FilePath
	, config	:: !Config
	, rivNick	:: !String
	, rivMap	:: !(Map String (Map String Status))
	, rivUptime	:: !Integer
	, rivGeoIP	:: !GeoIP.Data
	, rivPoll	:: !(Maybe (TremMasterCache.ServerCache, Integer))
	, rivPhost	:: !DNSEntry
	, rivTremded	:: (Maybe Socket, Maybe ThreadId)
	}

type RiverState = StateT River IO ()

type From = (NUH, String, String)
type Command = (String, String, String) -> RiverState

-- (name, (function, min arg required, min access level required, arguments, help info))
type CommandList = [(String, CommandInfo)]
type CommandInfo = (Command, Int, Access, String, String)

-- A nick that will never be in use.
initNick_ :: String
initNick_ = "\1\2\3\4\5\6\7\8NICK"
