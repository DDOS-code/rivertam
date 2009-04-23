module ComTremRelay where
import Network.Socket

import RiverState
import Config
import Send
import TremLib
import Helpers

-- TODO: Implement some restart thing, so everything can be reinitialized from the config.
-- Should be trivial.

#ifdef norelay
initRelay :: Config -> TChan String -> IO (Maybe Socket, Maybe ThreadId)
initRelay _ _ = return (Nothing, Nothing)

exitRelay :: (Maybe Socket, Maybe ThreadId) -> IO ()
exitRelay _ = return ()

ircToTrem :: String -> String -> String -> RiverState
ircToTrem _ _ _ = return ()

#else

initRelay :: Config -> TChan String -> IO (Maybe Socket, Maybe ThreadId)
initRelay config tchan = do
	tid <- if (not $ null $ tremdedchan config) && (not $ null $ tremdedfifo config) then
		Just `liftM` (forkIO $ tremToIrc tchan (tremdedchan config) (tremdedfifo config))
		else return Nothing

	sock <- if not $ null $ tremdedhost config then
			Just `liftM` initSock (tremdedhost config)
			else return Nothing

	return (sock, tid)


exitRelay :: (Maybe Socket, Maybe ThreadId) -> IO ()
exitRelay (s, t) = do
	case s of
		Just a	-> sClose a
		_ 	-> return ()

	case t of
		Just a	-> killThread a
		_ 	-> return ()


initSock :: String -> IO Socket
initSock ipport = do
	let (ip, port) 	= getIP ipport
	host	<- head `liftM` getAddrInfo Nothing (Just ip) (Just port)
	sock	<- socket (addrFamily host) Datagram defaultProtocol
	connect sock (addrAddress host)
	return sock

ircToTrem :: String -> String -> String -> RiverState
ircToTrem channel sender mess = do
	(maybesock, _)				<- gets rivTremded
	whenJust maybesock $ \sock -> do
	Config {tremdedrcon, tremdedchan}	<- gets config
	when (tremdedchan =|= channel) $ case shavePrefix "trem: " mess of
		Nothing -> return ()
		Just a	-> do
			lift $ send sock line
			return ()
			where line = "\xFF\xFF\xFF\xFFrcon "++tremdedrcon++" chat ^7[^5IRC^7] "++sender++": ^2" ++ a

tremToIrc :: TChan String -> String -> FilePath -> IO ()
tremToIrc tchan ircchan fifo = do
	-- The fifo has to be opened in ReadWrite Mode to prevent it from reaching EOF right away.
	-- Nothing will ever be written to it however.
	hdl <- openFile fifo ReadWriteMode
	hSetBuffering hdl NoBuffering
	forever $ do
		tremline <- (removeColors . filter isPrint) `liftM` hGetLine hdl
		case breakList ": irc: " =<< shaveFirstPrefix ["] say: ", " say: "] tremline of
			Just (name, mess) ->
				atomically $ writeTChan tchan $
				"PRIVMSG " ++ ircchan ++ " :<[T] "++name++"> " ++ mess

			_ -> return ()


--Some functions to extract the info
shaveFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe [a]
shaveFirstPrefix	(p:ps)	lst	=
	 case shavePrefix p lst of
		Nothing -> shaveFirstPrefix ps lst
		a	-> a
shaveFirstPrefix	[]	_	= Nothing

breakList :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakList at lst = splitX =<< infixAt at lst
	where splitX pos = Just $ (a, b)
		where
		(a, tmp)	= splitAt pos lst
		b 		= drop (length at) tmp

infixAt :: (Num n, Eq a) => [a] -> [a] -> Maybe n
infixAt inf = loop 0 where
	loop n		xx@(_:xs)	= if isPrefixOf inf xx then Just n else loop (n+1) xs
	loop _		[]		= Nothing

#endif
