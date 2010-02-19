module Rivertam (
	module River
	, rivertam
) where
import Network
import System.IO
import System.Directory
import Control.Exception
import qualified Data.Map as M
import Data.List

import River
import Module
import Irc.Protocol
import Irc.State
import Irc.OnEvent
import Send
import Command


rivertam :: Hooks x -> IO ()
rivertam hooks = withSocketsDo $ do
	execRiver run =<< initialize hooks
	return ()

initialize :: Hooks x -> IO (RState x)
initialize hooks@Hooks{comHook} = do
	path		<- getConfigPath "rivertam/"
	putStrLn ("!!! Config Path: " ++ show path)
	let riverconf	= path ++ "river.conf"

	config_		<- getConfig <$> readFile riverconf
	config 		<- either (\e -> error $ "river.conf: " ++ e) return config_

	sock		<- connectTo (network config) (PortNumber (port config))
	hSetBuffering sock NoBuffering

	sendchan 	<- atomically newTChan

	forkIO $ senderThread sock sendchan
	--forkIO $ forever $ (atomically . writeTChan sendchan) =<< getLine

	initTime	<- getUnixTime
	com		<- comHook config

	return RState {ircState=Irc.State.initial, commands = M.empty, ..}

run :: River x ()
run = do
	sendM =<< Irc.OnEvent.init <$> gets config
	mapM_ modInit =<< getActiveModules
	id =<< gets (initHook . hooks)
	r <- catchR (whileTrue mainloop >> return Nothing) (return . Just)
	case r of
		Just (e::SomeException) -> do
			trace (show e)
			io . atomically . clearSender =<< gets sendchan
			id =<< gets (quitHook . hooks)
		Nothing	-> trace "Clean exit perhaps?"

	mapM_ modFinish =<< getActiveModules
	io . hClose =<< gets sock

mainloop :: River x Bool
mainloop = do
	response <- io . try . hGetLine =<< gets sock
	case response of
		Left (e :: IOException) -> do
			trace "Socket closed"
			trace (show e)
			return False

		Right r -> do
			let line = dropWhileRev isSpace r
			echo $ "\x1B[32;1m>>\x1B[30;0m " ++ show line
			case ircToMessage line of
				Nothing -> return True
				Just a -> do
					modify $ \x -> x {ircState = Irc.State.update a (ircState x)}
					sendM =<< Irc.OnEvent.respond <$> gets config <*> pure a <*> gets ircState
					(\x -> x a) =<< gets (eventHook . hooks)
					commandHook a
					return True


whileTrue :: (Monad m) => m Bool -> m ()
whileTrue f = f >>= \c -> if c then whileTrue f else return ()

commandHook :: Message -> River x ()
commandHook (Message (NUH prefix@(Name nick user host)) (Msg reciever msg)) = do
	ircNick <- gets (ircNick . ircState)
	Config{comkey, access, queryaccess} <- gets config
	let	cPrefixes	= [comkey, recase ircNick++", ", recase ircNick++": "]
		gotaccess	= getAccess prefix access
		domain		= recase user ++ '@':recase host

	case findprefix cPrefixes msg of
		Just a  | ircNick /= reciever		-> command gotaccess reciever nick domain a
			| gotaccess >= queryaccess	-> command gotaccess nick nick domain a
		_       				-> return ()
commandHook _ = return ()

findprefix :: [String] -> String -> Maybe String
findprefix	[]	_	= Nothing
findprefix	(x:xs)	input	= case stripPrefixWith toLower x input of
					Nothing -> findprefix xs input
					Just ""	-> Nothing
					-- protect against sending no command errors to "!!!!" for example.
					Just (a:_) | (not . isAlphaNum) a -> Nothing
					a	-> a

getAccess :: Name -> [(Access, Name)] -> Access
getAccess who access = maybe Peon fst $ find (\(_, n) -> n == who) access

getConfigPath :: FilePath -> IO FilePath
getConfigPath name = do
	path 	<- getAppUserDataDirectory name
	t	<- doesDirectoryExist path
	return $! if t then path else ""
