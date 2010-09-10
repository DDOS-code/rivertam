module Module.TremRelay (mdl) where
import Module hiding (send)
import Module.State
import Network.Socket
import Control.Strategies.DeepSeq
import Control.Exception
import Control.Monad
import System.IO
import Send
import Irc.Protocol
import Tremulous.Util


mdl :: Module State
mdl = Module
	{ modName	= "tremrelay"
	, modInit	= do
		Config{tremdedhost, tremdedchan, tremdedfifo} <- gets config
		sendchan	<- gets sendchan
		sock		<- mInit [tremdedhost] $ initSock tremdedhost
		relay		<- mInit [recase tremdedchan, tremdedfifo] $ forkIO $ tremToIrc (sendIrc sendchan) tremdedchan tremdedfifo
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
	where	sendIrc chan = writeChan chan . strict . responseToIrc
		mInit c f = if all (not . null) c then Just <$> io f else return Nothing


initSock :: String -> IO Socket
initSock ipport = do
	let (ip, port) 	= getIP ipport
	host	<- getDNS ip port
	sock	<- socket (dnsFamily host) Datagram defaultProtocol
	connect sock (dnsAddress host)
	return sock

comRelay :: Command State
comRelay mess = do
	TremRelay maybesock _	<- getsCom relay
	Nocase sender		<- asks nickName
	rcon			<- gets (tremdedrcon . config)
	tremchan		<- gets (tremdedchan . config)
	okey			<- (tremchan /=) <$> asks channel
	case maybesock of
		Nothing	 -> Error >>> "Deactivated."
		Just _ | okey	-> Error >>> "Only active for " ++ recase tremchan ++ "."
		Just sock -> do
			io $ send sock $ "\xFF\xFF\xFF\xFFrcon "++rcon++" chat ^7[^5IRC^7] "++sender++": ^2" ++ mess
			return ()

-- The fifo has to be opened in ReadWriteMode to prevent it from reaching EOF right away.
-- Nothing will ever be written to it however.
tremToIrc :: (IRC -> IO ()) -> Nocase -> FilePath -> IO ()
tremToIrc echo ircchan fifo = bracket (openFile fifo ReadWriteMode) hClose $ \hdl -> do
	hSetBuffering hdl NoBuffering
	forever $ do
		tremline <- dropWhile invalid `liftM` hGetLine hdl
		case uncurry tline =<< fbreak tremline of
			Nothing	-> return ()
			Just a	-> echo $ Msg ircchan a

	where	
	fbreak x = case break (==':') x of
		(a, ':':' ':b)	-> Just (a, b)
		_		-> Nothing
	invalid x = isSpace x || x == ']' || isControl x

--For 1.2 
--Say: 0 "^5C^7adynum^5.^7ddos^7": ^2irc: lol
tline :: String -> String -> Maybe String
tline "Say" x = case stripInfix ": ^2irc: " . dropOneWord $ x of
	Just (name, m)	-> Just $ "<[T] " ++ ircifyColors name ++ "> " ++ removeColors m
	Nothing		-> Nothing

tline "say" x = case stripInfix ": irc: " x of
	Just (name, m)	-> Just $ "<[T] " ++ ircifyColors name ++ "> " ++ removeColors m
	Nothing		-> Nothing


tline _ _ = Nothing

dropOneWord :: String -> String
dropOneWord = dropWhile isSpace . dropWhile (not . isSpace) 
