module TremMasterCache (
	  ServerCache
	, ServerInfo
	, PlayerInfo
	, tremulousPollAll
) where
import Control.Monad
import Network.Socket
import System.Timeout
import qualified Data.Map as M
import Data.Map(Map)
import Control.Exception
import Data.Bits
import Control.Concurrent

import Helpers

type ServerMap = Map SockAddr (Maybe String)
type ServerCache = (Map SockAddr ServerInfo, Int)
type ServerInfo = ([(String, String)], PlayerInfo)
type PlayerInfo = [(Char, Int, Int, String)]

deriving instance Ord SockAddr

mastertimeout, polltimeout :: Integer
mastertimeout = 300*1000
polltimeout = 400*1000

recvTimeout :: Socket -> Integer -> IO [(String, Int, SockAddr)]
recvTimeout sock tlimit = loop =<< getMicroTime where
	loop start = do
		now <- getMicroTime
		let diff = now-start
		if diff < tlimit then do
			stuff <- timeout (fromInteger diff) $ recvFrom sock 1500
			case stuff of
				Just a 	-> (a:) `liftM` loop start
				Nothing	-> loop start
			else return []

masterGet :: Socket -> SockAddr -> IO [SockAddr]
masterGet sock masterhost = do
	response <- recvTimeout sock mastertimeout
	return $ cycleoutIP . concat . catMaybes $ [isProper mess | (mess, _, host) <- response, host == masterhost]
	where
		isProper = shaveOfContainer "\xFF\xFF\xFF\xFFgetserversResponse" "\\EOT\0\0\0"

serversGet :: Socket -> ServerMap -> IO ServerMap
serversGet sock themap_ = (loop themap_) `liftM` recvTimeout sock polltimeout
	where	loop themap [] = themap
		loop themap ((a, _, host):xs) = case M.lookup host themap of
			Just _ -> loop (M.insert host (isProper a) themap) xs
			Nothing -> loop themap xs
		isProper = shavePrefix "\xFF\xFF\xFF\xFFstatusResponse"

serversGetResend ::	Int ->	Socket -> ServerMap -> IO ServerMap
serversGetResend 	0	_	servermap	= return servermap
serversGetResend 	_	_	servermap
	| (M.size $ M.filter isNothing servermap) == 0 	= return servermap
serversGetResend 	n	sock	servermap 	= do
	let (sJust, sNothing) = (M.filter isJust servermap, M.filter isNothing servermap)
	mapM_ (sendTo sock "\xFF\xFF\xFF\xFFgetstatus") [a | (a, _) <- M.toList sNothing]
	response <- serversGet sock servermap
	serversGetResend (n-1) sock (M.unionWith priojust response sJust)
	where
		priojust	(Just a)	Nothing		= Just a
		priojust	Nothing		(Just a)	= Just a
		priojust	(Just a)	(Just _)	= Just a
		priojust	Nothing		Nothing		= Nothing

tremulousPollAll :: AddrInfo -> IO ServerCache
tremulousPollAll host =
	bracket (socket (addrFamily host) Datagram defaultProtocol) sClose $ \sock -> do
		print "startpoll"
		sp <- getMicroTime
		sendTo sock "\xFF\xFF\xFF\xFFgetservers 69 empty full" (addrAddress host)
		ep <- getMicroTime
		print "sendtmaster"
		print $ (ep-sp) // 1000
		masterresponse <- masterGet sock (addrAddress host)
		ep <- getMicroTime
		print "master"
		print $ (ep-sp) // 1000
		let servermap = M.fromList [(a, Nothing) | a <- masterresponse]
		ep <- getMicroTime
		print "firstlist"
		print $ (ep-sp) // 1000
		polledMaybe <- (M.map pollFormat) `liftM` serversGetResend 3 sock servermap
		ep <- getMicroTime
		print "polled"
		print $ (ep-sp) // 1000
		let (polled, unresponsive) = (M.map (fromJust) $ M.filter isJust polledMaybe, M.size polledMaybe - M.size polled)
		ep <- getMicroTime
		print "stoppoll"
		print $ (ep-sp) // 1000
		return (polled, unresponsive)


cycleoutIP :: String -> [SockAddr]
cycleoutIP [] = []
cycleoutIP strs = sockaddr:cycleoutIP ss where
	(ff, ss)		= splitAt 7 strs
	(ip, port)		= splitAt 4 (drop 1 ff)
	sockaddr		= SockAddrInet (toPort port) (toIP ip)
	toPort (a:b:[])		= fromIntegral $ (ord a `shiftL` 8) + ord b
	toPort _		= 0
	toIP (d:c:b:a:[])	= fromIntegral $ (ord a `shiftL` 24) + (ord b `shiftL` 16) + (ord c `shiftL` 8) + ord d
	toIP _			= 0

pollFormat :: Maybe String -> Maybe ServerInfo
pollFormat Nothing = Nothing
pollFormat (Just []) = Nothing
pollFormat (Just line) = return (cvars, players) where
		(cvars_:players_)	= splitlines line
		cvars			= cvarstuple . split (=='\\') $ cvars_
		players			= playerList (players_) $ fromMaybe (repeat '9') $ lookup "P" cvars

playerList :: [String] -> String -> PlayerInfo
playerList pa@(p:ps) (l:ls)  =
	case l of
		'-'	->	playerList pa ls
		team	->	(team, fromMaybe 0 (mread kills), fromMaybe 0 (mread ping), name') : playerList ps ls
	where
		(kills, buf)	= break isSpace p
		(ping, name)	= break isSpace (stripw buf)
		name'		= stripw $ filter (/='"') name
playerList _ _ = []

cvarstuple :: [String] -> [(String, String)]
cvarstuple (c:v:ss)	= (c, v) : cvarstuple ss
cvarstuple _		= []
