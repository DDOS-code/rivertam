module CommandInterface (
	  module Control.Monad.State
	, Info(..)
	, ComResponse(..)
	, ComState(..)
	, PollData(..)
	, Countdown(..)
	, CountdownType
	, Transformer
	, Command
	, CommandList
	, CommandInfo
	, echo
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
	  echoFunc2	:: (ComResponse -> IO ())
	, filePath	:: FilePath
	, config2	:: Config
	, myNick	:: String
	, userList	:: [String]
	}

data ComResponse = Mess !String | Private !String

type Transformer	= StateT ComState IO
type Command		= String -> String -> Info -> Transformer ()
type CommandList	= [(String, CommandInfo)]
type CommandInfo	= (Command, Int, Access, String, String)

echo :: ComResponse -> StateT ComState IO ()
echo a = do
	f <- gets echoFunc
	lift $ f a

-- Shitty part that depends on Other Stuff
data ComState = ComState {
	  echoFunc	:: (ComResponse -> IO ())
	, uptime	:: Integer
	, geoIP		:: GeoIP

	, poll		:: !PollData
	, pollHost	:: !DNSEntry

	, counter	:: Int
	, countdownS	:: CountdownType
	}

type CountdownType = TVar (Map Int (String, Countdown, ThreadId))
data PollData = PollData !TremMasterCache.ServerCache !Integer | PollNone
data Countdown = Countdown Integer String String

instance Read Countdown where
	readPrec = do
		Int time	<- lexP
		String comment	<- lexP
		String final	<- lexP
		return $ Countdown time comment final
