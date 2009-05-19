module CommandInterface (
	  module Control.Monad.State
	, Info(..)
	, ComState(..)
	, Countdown(..)
	, CountdownType
	, Transformer
	, Command
	, CommandList
	, CommandInfo
) where
import Control.Monad.State

import Config
import Helpers
import TremMasterCache

import GeoIP
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Text.Read hiding (lift)

data Info = Info {
	  echo
	, echop		:: (String -> IO ())
	, filePath	:: FilePath
	, config2	:: Config
	, myNick	:: String
	, userList	:: [String]
	}

type Transformer	= StateT ComState IO
type Command		= String -> String -> Info -> Transformer ()
type CommandList	= [(String, CommandInfo)]
type CommandInfo	= (Command, Int, Access, String, String)


-- Shitty part that depends on Other Stuff
data ComState = ComState {
	  uptime	:: Integer
	, geoIP		:: GeoIP

	, poll		:: !TremMasterCache.ServerCache
	, pollTime	:: !Integer
	, pollHost	:: !DNSEntry

	, counter	:: Int
	, countdownS	:: CountdownType
	}

type CountdownType = TVar (Map Int (String, Countdown, ThreadId))
data Countdown = Countdown Integer String String

instance Read Countdown where
	readPrec = do
		Int time	<- lexP
		String comment	<- lexP
		String final	<- lexP
		return $ Countdown time comment final
