module CommandInterface (
	module River
	, module RiverHDBC
	, module Config
	, module Database.HDBC
	, module Control.Monad.Reader.Class
	, HolderTrem(..), TremRelay(..), CState(..), initialCState
	, Info(..), RiverCom(..)
	, Module(..), Command, CommandList, CommandInfo
	, runRiverCom, catchRC
	, getsCom, modifyCom
	, getUserList, getRiverNick
	, sqlTransactionTry, sqlTransaction, sqlIfNotTable
	, SendType(..), (>>>), view
) where
import River
import RiverHDBC
import Database.HDBC
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Exception
import Prelude hiding (catch)
import Data.Foldable (Foldable)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import IRC
import IrcState
import Config

import Control.Concurrent
import TremPolling
import GeoIP
import Network.Socket (Socket)

type Command		= String -> RiverCom ()
type CommandList	= [(String, CommandInfo)]
type CommandInfo	= (Command, Int, Access, String, String)

data Info = Info
	{ userAccess	:: !Access
	, channel	:: !Nocase
	, commandName	:: !String
	, nickName	:: !Nocase
	, domain	:: !String
	}

data HolderTrem = HolderTrem !Integer DNSEntry !PollResponse
data TremRelay = TremRelay !(Maybe Socket) !(Maybe ThreadId)

data CState = CState
	{ commands	:: !(Map String CommandInfo)
	, geoIP		:: GeoIP
	, trempoll	:: HolderTrem
	, relay		:: TremRelay
	}

data Module = Module
	{ modName	:: !String
	, modInit	:: !(River CState ())
	, modFinish	:: !(River CState ())
	, modList	:: !CommandList
	}

newtype RiverCom a = RiverCom (ReaderT Info (River CState) a)
	deriving (Functor, Monad, MonadIO, MonadState (RState CState), MonadReader Info)

instance Applicative RiverCom where
	pure = return
	(<*>) = ap

runRiverCom :: RiverCom r -> Info -> RState CState -> IO (r, RState CState)
runRiverCom (RiverCom rc) r = runRiver (runReaderT rc r)

catchRC :: Exception e => RiverCom r -> (e -> RiverCom r) -> RiverCom r
catchRC f errf = do
    state	<- get
    reader	<- ask
    (a, state')	<- io $ runRiverCom f reader state `catch` \e -> runRiverCom (errf e) reader state
    put state'
    return a

modifyCom :: (MonadState (RState x) m) => (x -> x) -> m ()
modifyCom f = modify $ \x -> x {com = f (com x)}

getsCom :: (MonadState (RState x) m) => (x -> c) -> m c
getsCom x = gets (x . com)

initialCState :: CState
initialCState = CState M.empty undefined undefined undefined

data (Foldable f, Functor f) => SendType f = Echo String | EchoM (f String) | Whisper String | Error String

infixr 0 >>>

(>>>) :: (t -> SendType []) -> t -> RiverCom ()
stype >>> mess = do
	chan <- asks channel
	name <- asks commandName
	case stype mess of
		Echo a		-> send $ Msg chan a
		EchoM a		-> sendM $ fmap (Msg chan) a
		Whisper a	-> send $ Notice chan a
		Error a		-> send $ Msg chan (view name a)

getUserList :: RiverCom (Map Nocase Status)
getUserList = do
	ircMap	<- gets (ircMap . ircState)
	channel	<- asks channel
	return $ fromMaybe M.empty (M.lookup channel ircMap)

getRiverNick :: RiverCom Nocase
getRiverNick = gets (ircNick . ircState)

view :: String -> String -> String
view x v = '\STX' : x ++ ":\STX " ++ v


-- Sql functions -----------------------------------------------------------------------------------

sqlTransactionTry :: RiverCom a -> (SqlError -> RiverCom a) -> RiverCom a
sqlTransactionTry f errf = catchRC toTry toFail
	where
	toTry = do
		x <- f
		sqlArg0 commit
		return x
	toFail e = do
		ignoreException (sqlArg0 rollback)
		x <- errf e
		return x
	ignoreException x = catchRC x $ \(_ :: SqlError) -> return ()


sqlTransaction :: RiverCom a -> RiverCom a
sqlTransaction f = catchRC toTry toFail where
	toTry = do x <- f; sqlArg0 commit; return x
	toFail :: SqlError -> RiverCom a
	toFail e = sqlArg0 rollback >> throw e

sqlIfNotTable :: String -> [String] -> River x ()
sqlIfNotTable tbl x = do
	tables <- sqlArg0 getTables
	unless (tbl `elem` tables) $ mapM_ (\a -> sqlRun a [] >> sqlArg0 commit) x
