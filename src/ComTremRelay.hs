module ComTremRelay (mdl) where
import Network.Socket
import Control.Strategies.DeepSeq
import Control.Exception
import System.IO
import Data.List
import IRC

import TremLib
import Send

import CommandInterface hiding (send, echo)

mdl :: Module
mdl = Module
	{ modName	= "tremrelay"
	, modInit	= do
		Config{tremdedhost, tremdedchan, tremdedfifo} <- gets config
		sendchan <- gets sendchan
		sock	<- mInit [tremdedhost] $ initSock tremdedhost
		relay	<- mInit [recase tremdedchan, tremdedfifo] $ forkIO $ tremToIrc (sendIrc sendchan) tremdedchan tremdedfifo
		modifyCom $ \x -> x {relay = TremRelay sock relay}

	, modFinish	= do
		(TremRelay s t) <- getsCom relay
		let g f = maybe (return ()) (io . f)
		g sClose s
		g killThread t
		modifyCom $ \x -> x {relay = TremRelay Nothing Nothing}

	, modList	= [("trem"	, (comRelay	, 1	, Peon		, "<<message>>"
				, "Send a message to a trem server."))]
	}
	where	sendIrc tchan = atomically . writeTChan tchan . strict . responseToIrc
		mInit c f = if all (not . null) c then Just <$> io f else return Nothing


initSock :: String -> IO Socket
initSock ipport = do
	let (ip, port) 	= getIP ipport
	host	<- getDNS ip port
	sock	<- socket (dnsFamily host) Datagram defaultProtocol
	connect sock (dnsAddress host)
	return sock

comRelay :: Command
comRelay mess = do
	TremRelay maybesock _	<- getsCom relay
	Nocase sender		<- asks nickName
	rcon		<- gets (tremdedrcon . config)
	tremchan	<- gets (tremdedchan . config)
	okey		<- (tremchan /=) <$> asks channel
	case maybesock of
		Nothing	 -> Error >>> "Deactivated."
		Just _ | okey	-> Error >>> "Only active for " ++ recase tremchan ++ "."
		Just sock -> do
			io $ send sock $ "\xFF\xFF\xFF\xFFrcon "++rcon++" chat ^7[^5IRC^7] "++sender++": ^2" ++ mess
			return ()

-- The fifo has to be opened in ReadWriteMode to prevent it from reaching EOF right away.
-- Nothing will ever be written to it however.
tremToIrc :: (Response -> IO ()) -> Nocase -> FilePath -> IO ()
tremToIrc echo ircchan fifo = bracket (openFile fifo ReadWriteMode) hClose $ \hdl -> do
	hSetBuffering hdl NoBuffering
	forever $ do
		tremline <- dropWhile invalid `liftM` hGetLine hdl
		maybe (return ()) (echo . Msg ircchan) $ uncurry tline =<< fbreak tremline

	where	fbreak x = case break (==':') x of
			(a, ':':' ':b)	-> Just (a, b)
			_		-> Nothing
		invalid x = isSpace x || x == ']' || isControl x

tline :: String -> String -> Maybe String
tline "say" x = case stripInfix ": irc: " x of
	Just (name, m)	-> Just $ "<[T] " ++ ircifyColors name ++ "> " ++ removeColors m
	Nothing		-> Nothing

--TODO: take teamkilling into account.
--"Kill: 0 1 23: Entroacceptor.ddos killed Cadynum(eVo)ddos by MOD_ABUILDER_CLAW"
tline "Kill" mess = let
	mess' = dropWhile (not . isAlpha) mess
	in untilJust (match mess') messages
	where
	match l (a, b) = case stripSuffix a l of
		Just x	-> Just $ ircifyColors x ++ b
		Nothing	-> Nothing


tline _ _ = Nothing


messages :: [(String, String)]
messages = [
	  ("by MOD_SLOWBLOB"		, "with \STXgranger spit!\STX")
	-- , ("by MOD_ABUILDER_CLAW"	, "with a granger.")
	-- , ("by MOD_BLASTER"		, "with a blaster.")
	]

untilJust :: (t -> Maybe a) -> [t] -> Maybe a
untilJust _ []		= Nothing
untilJust f (x:xs)	= maybe (untilJust f xs) Just $ f x
