module River(
	module Control.Applicative
	, module Control.Monad.State
	, module Helpers
	, module Config
	, HolderTrem(..), TremRelay(..), RState(..), River(..)
	, runRiver, execRiver, catchR, send, sendM, echo, trace, io
) where
import Prelude hiding (catch, mapM_)
import Control.Applicative
import Control.Monad.State hiding (mapM_)
import Control.Exception
import Control.Strategies.DeepSeq
import Data.Foldable

import Database.HDBC.PostgreSQL
import System.IO
import System.Time
import Send
import IRC
import IrcState
import Helpers
import Config

import TremPolling
import GeoIP
import Network.Socket (Socket)

data HolderTrem = HolderTrem !Integer DNSEntry !PollResponse
data TremRelay = TremRelay !(Maybe Socket) !(Maybe ThreadId)

data RState = RState
	{ sock		:: !Handle
	, sendchan	:: !SenderChan
	, conn		:: !Connection
	, initTime	:: !Integer
	, config	:: !Config
	, configPath	:: !FilePath
	, configTime	:: !ClockTime
	, ircState	:: !IrcState

	, comTrem	:: !HolderTrem
	, tremRelay	:: !TremRelay
	, geoIP		:: !GeoIP
	}

newtype River a = River (StateT RState IO a)
	 deriving (Functor, Monad, MonadIO, MonadState RState)

instance Applicative River where
	pure = return
	(<*>) = ap

runRiver :: River r -> RState -> IO (r, RState)
runRiver (River r) = runStateT r

execRiver :: River r -> RState -> IO RState
execRiver (River r) = execStateT r

catchR :: Exception e => River r -> (e -> River r) -> River r
catchR f errf = do
    state	<- get
    (a, state')	<- io $ runRiver f state `catch` \e -> runRiver (errf e) state
    put state'
    return a

send :: (MonadState RState m, MonadIO m) => Response -> m ()
send x = gets sendchan >>= \c -> io $ sender c x
	where sender c = atomically . writeTChan c . strict . responseToIrc

sendM :: (MonadState RState m, MonadIO m, Foldable f) => f Response -> m ()
sendM x = gets sendchan >>= \c -> io $ atomically $ mapM_ (sender c) x
	where sender c = writeTChan c . strict . responseToIrc

echo :: (MonadState RState m, MonadIO m) => String -> m ()
echo x = gets (debug . config) >>= \d -> when (d >= 1) (io $ putStrLn x)

trace :: MonadIO m => String -> m ()
trace x = io $ hPutStrLn stderr x >> hFlush stderr

io :: MonadIO m => IO a -> m a
io = liftIO
