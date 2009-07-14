module TremRelay (
	TremRelay(..)
	, initialize
	, finalize
	, list
) where
import Network.Socket
import Control.Monad
import System.IO
import Control.Concurrent
import Data.List
import IRC

import Config
import Helpers
import TremLib

import CommandInterface
import Data.IORef

list :: CommandList
list = 	[("trem"	, (comRelay	, 1	, Peon		, "<<message>>"
		, "Send a message to a trem server."))]

-- TODO: Implement some restart thing, so everything can be reinitialized from the config.
-- Should be trivial.

initialize :: Config -> (Response -> IO ()) -> IO TremRelay
initialize Config{tremdedhost, tremdedchan, tremdedfifo} echo = liftM2 TremRelay
	(unlessc [tremdedhost] $
		initSock tremdedhost)
	(unlessc [tremdedchan, tremdedfifo] $
		forkIO $ tremToIrc echo tremdedchan tremdedfifo)

	where unlessc cond f
		| all (not . null) cond		= Just `liftM` f
		| otherwise 			= return Nothing

finalize :: TremRelay -> IO ()
finalize (TremRelay s t) = do
	case s of
		Just a	-> sClose a
		_ 	-> return ()
	case t of
		Just a	-> killThread a
		_ 	-> return ()

initSock :: String -> IO Socket
initSock ipport = do
	let (ip, port) 	= getIP ipport
	host	<- getDNS ip port
	sock	<- socket (dnsFamily host) Datagram defaultProtocol
	connect sock (dnsAddress host)
	return sock

comRelay :: Command
comRelay sender mess Info{echo, config} ComState{relay} = do
	TremRelay maybesock _	<- readIORef relay
	case maybesock of
		Nothing	-> echo $ "Irc -> Trem: Deactivated."
		Just sock -> do
			send sock $ "\xFF\xFF\xFF\xFFrcon "++rcon++" chat ^7[^5IRC^7] "++sender++": ^2" ++ mess
			return ()
	where Config {tremdedrcon=rcon} = config

tremToIrc :: (Response -> IO ()) -> String -> FilePath -> IO ()
tremToIrc echo ircchan fifo = do
	-- The fifo has to be opened in ReadWriteMode to prevent it from reaching EOF right away.
	-- Nothing will ever be written to it however.
	hdl <- openFile fifo ReadWriteMode
	hSetBuffering hdl NoBuffering
	forever $ do
		tremline <- dropWhile (not . isAlpha) `liftM` hGetLine hdl
		maybe (return ()) (echo . Msg ircchan) $ uncurry tline =<< fbreak tremline

	where fbreak x = case break (==':') x of
				(a, ':':' ':b)	-> Just (a, b)
				_		-> Nothing

tline :: String -> String -> Maybe String
tline "say" x = case shaveInfix ": irc: " x of
	Just (name, m)	-> Just $ "<[T] " ++ ircifyColors name ++ "> " ++ removeColors m
	Nothing		-> Nothing

--TODO: take teamkilling into account.
--"Kill: 0 1 23: Entroacceptor.ddos killed Cadynum(eVo)ddos by MOD_ABUILDER_CLAW"
tline "Kill" mess = let
	mess' = dropWhile (not . isAlpha) mess
	in untilJust (match mess') messages
	where
	match l (a, b) = case shaveSuffix a l of
		Just x	-> Just $ ircifyColors x ++ b
		Nothing	-> Nothing


tline _ _ = Nothing


messages :: [(String, String)]
messages = [
	  ("by MOD_SLOWBLOB"		, "with \STXgranger spit!\STX")
	, ("by MOD_ABUILDER_CLAW"	, "with a granger.")
	, ("by MOD_BLASTER"		, "with a blaster.")
	]

untilJust :: (t -> Maybe a) -> [t] -> Maybe a
untilJust _ []		= Nothing
untilJust f (x:xs)	= maybe (untilJust f xs) Just $ f x
