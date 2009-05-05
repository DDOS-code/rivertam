module RiverState (
	  module System.IO
	, module Control.Monad.State
	, Status(..)
	, PollData(..)
	, River(..)
	, RiverState
	, From
	, Command
	, CommandList
	, CommandInfo
) where
import System.IO
import Control.Monad.State
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Network.Socket

import Config
import Helpers
import IRC

import qualified TremMasterCache
import GeoIP (GeoIP)


data PollData = PollData !TremMasterCache.ServerCache !Integer | PollNone

data River = River
	{ rivSender	:: !(TChan String)
	, rivSocket	:: !Handle
	, rivConfDir	:: !FilePath
	, config	:: !Config
	, rivNick	:: !String
	, rivMap	:: !(Map String (Map String Status))
	, rivUptime	:: !Integer
	, rivGeoIP	:: !GeoIP
	, rivPoll	:: !PollData
	, rivPhost	:: !DNSEntry
	, rivTremded	:: (Maybe Socket, Maybe ThreadId)
	}

type RiverState = StateT River IO ()

type From = (String, String, String)
type Command = (String, String, String) -> RiverState

-- (name, (function, min arg required, min access level required, arguments, help info))
type CommandList = [(String, CommandInfo)]
type CommandInfo = (Command, Int, Access, String, String)
