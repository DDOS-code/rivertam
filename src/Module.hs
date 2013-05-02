module Module (
	  module Control.Monad.Reader.Class
	, module River
	, module Config
	, runRiverCom, catchRC
	, getsCom, modifyCom
	, getUserList, getRiverNick
	, SendType(..), (>>>), view, getActiveModules
) where
import River
import Prelude hiding (catch)
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Exception
import Data.Foldable (Foldable)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Config
import Irc.Protocol
import Irc.State

runRiverCom :: RiverCom x r -> Info -> RState x -> IO (r, RState x)
runRiverCom (RiverCom rc) r = runRiver (runReaderT rc r)

catchRC :: Exception e => RiverCom x r -> (e -> RiverCom x r) -> RiverCom x r
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

data (Foldable f, Functor f) => SendType f =
	  Echo String
	| EchoM (f String)
	| Whisper String
	| Error String

infixr 0 >>>

(>>>) :: (t -> SendType []) -> t -> RiverCom x ()
stype >>> mess = do
	Info{channel=chan, commandName=name, nickName=nick} <- ask
	case stype mess of
		Echo a		-> send $ Msg chan a
		EchoM a		-> sendM $ fmap (Msg chan) a
		Whisper a | chan == nick	-> send $ Msg nick a
			  | otherwise		-> send $ Notice nick a
		Error a		-> send $ Msg chan (view name a)

getUserList :: RiverCom x (Map Nocase Status)
getUserList = do
	ircMap	<- gets (ircMap . ircState)
	channel	<- asks channel
	return $ fromMaybe M.empty (M.lookup channel ircMap)

getRiverNick :: RiverCom x Nocase
getRiverNick = gets (ircNick . ircState)

view :: String -> String -> String
view x v = '\STX' : x ++ ":\STX " ++ v

getActiveModules :: (MonadState (RState x) m) => m [Module x]
getActiveModules = do
	exclude <- filter (/="core") `liftM` gets (modulesexcl . config)
	filter (flip notElem exclude . modName) `liftM` gets (moduleHook . hooks)
