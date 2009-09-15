module CommandInterface (
	module Config
	, module Helpers
	, module IRC
	, Info(..)
	, ComState(..)
	, Command
	, CommandList
	, CommandInfo
	, TremRelay(..)
) where
import Config
import Helpers
import Data.IORef
import Data.Map (Map)
import Database.HDBC.PostgreSQL

import TremPolling
import GeoIP
import Control.Concurrent
import Network.Socket
import IRC (Name(..))

data Info = Info {
	  echo
	, echop		:: String -> IO ()
	, filePath	:: FilePath
	, config	:: Config
	, myNick	:: Nocase
	, userList	:: Map Nocase ()
	}

type Command		= Name -> String -> Info -> ComState -> IO ()
type CommandList	= [(String, CommandInfo)]
type CommandInfo	= (Command, Int, Access, String, String)


-- Shitty part that depends on Other Stuff
data ComState = ComState {
	  conn		:: !Connection
	, uptime	:: !Integer
	, geoIP		:: !GeoIP

	, poll		:: !(IORef PollResponse)
	, pollTime	:: !(IORef Integer)
	, pollHost	:: !(IORef DNSEntry)

	, relay		:: !(IORef TremRelay)
	}

data TremRelay = TremRelay !(Maybe Socket) !(Maybe ThreadId)
