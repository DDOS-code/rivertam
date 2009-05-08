module CommandInterface (
	  module Control.Monad.State
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
	, getFP
) where
import Control.Monad.State

import Config
import Helpers
import TremMasterCache

import GeoIP
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Text.Read hiding (lift)

data ComResponse = Mess !String | Private !String

type Transformer	= StateT ComState IO
type Command		= String -> String -> Transformer ()
type CommandList	= [(String, CommandInfo)]
type CommandInfo	= (Command, Int, Access, String, String)

echo :: ComResponse -> StateT ComState IO ()
echo a = do
	f <- gets echoFunc
	lift $ f a

getFP :: FilePath -> Transformer FilePath
getFP a = (\x -> return $ x++a) =<< gets filePath

-- Shitty part that depends on Other Stuff
data ComState = ComState {
	  echoFunc	:: (ComResponse -> IO ())
	, filePath	:: FilePath
	, conf		:: Config
	, myNick	:: String
	, userList	:: [String]

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
