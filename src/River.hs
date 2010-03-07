module River(
	module Control.Applicative
	, module Control.Monad.State.Class
	, module Helpers
	, Info(..), RiverCom(..) , Module(..), Command, CommandList, CommandInfo
	, Hooks(..), RState(..), River(..)
	, runRiver, execRiver, catchR, send, sendM, echo, trace, io
) where
import Prelude hiding (catch, mapM_)
import Control.Applicative
import Control.Monad.State hiding (mapM_)
import Control.Monad.State.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Exception
import Control.Strategies.DeepSeq
import Data.Foldable
import Data.Map (Map)

import System.IO
import Send
import Irc.Protocol
import Irc.State
import Helpers
import Config

type Command x		= String -> RiverCom x ()
type CommandList x	= [(String, CommandInfo x)]
type CommandInfo x	= (Command x, Int, Access, String, String)

data Info = Info
	{ userAccess	:: !Access
	, channel	:: !Nocase
	, commandName	:: !String
	, nickName	:: !Nocase
	, domain	:: !String
	}

data Module x = Module
	{ modName	:: !String
	, modInit	:: !(River x ())
	, modFinish	:: !(River x ())
	, modList	:: !(CommandList x)
	}


data RState x = RState
	{ sock		:: !Handle
	, sendchan	:: !SenderChan
	, initTime	:: !Integer
	, config	:: !Config
	, path		:: !FilePath
	, ircState	:: !IrcState
	, hooks		:: !(Hooks x)
	, commands	:: !(Map String (CommandInfo x))
	, com		:: !x
	}

data Hooks x = Hooks
	{ comHook 	:: !(Config -> IO x)
	, moduleHook	:: ![Module x]
	, initHook 	:: !(River x ())
	, quitHook 	:: !(River x ())
	, eventHook	:: !(Message -> River x ())
	, aliasHook	:: !(String -> RiverCom x (Maybe (String, String)))
	}

newtype River x a = River (StateT (RState x) IO a)
	 deriving (Functor, Monad, MonadIO, MonadState (RState x))

instance Applicative (River x) where
	pure = return
	(<*>) = ap

newtype RiverCom x a = RiverCom (ReaderT Info (River x) a)
	deriving (Functor, Monad, MonadIO, MonadState (RState x), MonadReader Info)

instance Applicative (RiverCom x) where
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

send :: (MonadState (RState x) m, MonadIO m) => IRC -> m ()
send x = gets sendchan >>= \c -> io $ sender c x
	where sender c = writeChan c . strict . responseToIrc

sendM :: (MonadState (RState x) m, MonadIO m, Foldable f) => f IRC -> m ()
sendM x = gets sendchan >>= \c -> io $ mapM_ (sender c) x
	where sender c = writeChan c . strict . responseToIrc

echo :: (MonadState (RState x) m, MonadIO m) => String -> m ()
echo x = gets (debug . config) >>= \d -> when (d >= 1) (io $ putStrLn x)

trace :: MonadIO m => String -> m ()
trace x = io $ hPutStrLn stderr x >> hFlush stderr

io :: MonadIO m => IO a -> m a
io = liftIO
