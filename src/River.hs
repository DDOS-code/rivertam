module River(
	module Control.Applicative
	, module Control.Monad.State
	, module Helpers
	, RState(..), River(..)
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

data RState x = RState
	{ sock		:: !Handle
	, sendchan	:: !SenderChan
	, conn		:: !Connection
	, initTime	:: !Integer
	, config	:: !Config
	, configPath	:: !FilePath
	, configTime	:: !ClockTime
	, ircState	:: !IrcState
	, com		:: !x
	}

newtype River x a = River (StateT (RState x) IO a)
	 deriving (Functor, Monad, MonadIO, MonadState (RState x))

instance Applicative (River x) where
	pure = return
	(<*>) = ap

runRiver :: River a r -> RState a -> IO (r, RState a)
runRiver (River r) = runStateT r

execRiver :: River a r -> RState a -> IO (RState a)
execRiver (River r) = execStateT r

catchR :: Exception e => River a r -> (e -> River a r) -> River a r
catchR f errf = do
    state	<- get
    (a, state')	<- io $ runRiver f state `catch` \e -> runRiver (errf e) state
    put state'
    return a

send :: (MonadState (RState x) m, MonadIO m) => Response -> m ()
send x = gets sendchan >>= \c -> io $ sender c x
	where sender c = atomically . writeTChan c . strict . responseToIrc

sendM :: (MonadState (RState x) m, MonadIO m, Foldable f) => f Response -> m ()
sendM x = gets sendchan >>= \c -> io $ atomically $ mapM_ (sender c) x
	where sender c = writeTChan c . strict . responseToIrc

echo :: (MonadState (RState x) m, MonadIO m) => String -> m ()
echo x = gets (debug . config) >>= \d -> when (d >= 1) (io $ putStrLn x)

trace :: MonadIO m => String -> m ()
trace x = io $ hPutStrLn stderr x >> hFlush stderr

io :: MonadIO m => IO a -> m a
io = liftIO
