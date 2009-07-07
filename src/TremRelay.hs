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
		tremline <- (removeColors . filter isPrint) `liftM` hGetLine hdl
		case shaveInfix ": irc: " =<< shaveFirstPrefix ["] say: ", " say: "] tremline of
			Just (name, mess) ->
				echo $ Msg ircchan $ "<[T] "++name++"> " ++ mess

			_ -> return ()


shaveFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe [a]
shaveFirstPrefix	(p:ps)	lst	=
	 case shavePrefix p lst of
		Nothing -> shaveFirstPrefix ps lst
		a	-> a
shaveFirstPrefix	[]	_	= Nothing
