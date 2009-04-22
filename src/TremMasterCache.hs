module TremMasterCache (
	  ServerCache
	, ServerInfo
	, PlayerInfo
	, tremulousPollAll
) where
import Control.Monad
import Network.Socket
--import Network.Socket.ByteString
import System.Timeout
import qualified Data.Map as M
import Data.Map(Map)
import Control.Exception

import Helpers

type ServerMap = Map SockAddr (Maybe String)
type ServerCache = (Map SockAddr ServerInfo, Int)
type ServerInfo = (Map String String, PlayerInfo)
type PlayerInfo = [(Char, Int, Int, String)]

deriving instance Ord SockAddr

mastersrv, masterport :: String
mastertimeout, polltimeout :: Integer
mastersrv =  "master.tremulous.net"
masterport = "30710"
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
serversGet sock themap = (loop themap) `liftM` recvTimeout sock polltimeout
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
		priojust (Just a) Nothing = Just a
		priojust Nothing (Just a) = Just a
		priojust (Just a) (Just _) = Just a

tremulousPollAll :: IO ServerCache
tremulousPollAll = do
	host <- head `liftM` getAddrInfo Nothing (Just mastersrv) (Just masterport)
	bracket (socket (addrFamily host) Datagram defaultProtocol) sClose $ \sock -> do
		sendTo sock "\xFF\xFF\xFF\xFFgetservers 69 empty full" (addrAddress host)
		masterresponse <- masterGet sock (addrAddress host)
		let servermap = M.fromList [(a, Nothing) | a <- masterresponse]
		polledMaybe <- (M.map pollFormat) `liftM` serversGetResend 3 sock servermap
		let (polled, unresponsive) = (M.map (fromJust) $ M.filter isJust polledMaybe, M.size polledMaybe - M.size polled)
		return (polled, unresponsive)

cycleoutIP :: String -> [SockAddr]
cycleoutIP [] = []
cycleoutIP strs = sockaddr:cycleoutIP ss where
	(ff, ss)		= splitAt 7 strs
	(ip, port)		= splitAt 4 (drop 1 ff)
	sockaddr		= SockAddrInet (toPort port) (toIP ip)
	toPort (a:b:[])		= fromIntegral $ ord a*2^8 + ord b
	toIP (d:c:b:a:[])	= fromIntegral $ ord a*2^24 + ord b*2^16 + ord c*2^8 + ord d

pollFormat :: Maybe String -> Maybe ServerInfo
pollFormat Nothing = Nothing
pollFormat (Just []) = Nothing
pollFormat (Just line) = return (cvars, players) where
		(cvars_:players_) = splitlines line
		cvars = M.fromList . cvarstuple . split (=='\\') $ cvars_
		players = playerList (players_) $ fromMaybe (repeat '9') $ M.lookup "P" cvars

playerList :: [String] -> String -> PlayerInfo
playerList [] _ = []
playerList pa@(p:ps) (l:ls)  =
	case l of
		'-'	->	playerList pa ls
		team	->	(team, fromMaybe 0 (mread kills), fromMaybe 0 (mread ping), name') : playerList ps ls
	where
		(kills, buf)	= break isSpace p
		(ping, name)	= break isSpace (stripw buf)
		name'		= stripw $ filter (/='"') name

cvarstuple :: [String] -> [(String, String)]
cvarstuple [] = []
cvarstuple (c:v:ss) = (c, v) : cvarstuple ss
