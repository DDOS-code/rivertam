module TremMasterCache (
	  ServerCache
	, ServerInfo
	, PlayerInfo
	, tremulousPollAll
) where
import Control.Monad
import Network.Socket
import qualified Data.Map as M
import Data.Map(Map)
import Data.Bits
import System.Timeout

import Helpers

type ServerMap = Map SockAddr (Maybe String)
type ServerCache = (Map SockAddr ServerInfo, Int)
type ServerInfo = ([(String, String)], PlayerInfo)
type PlayerInfo		= [(Char	-- Team ('0'=spec, '1'=alien, '2'=human, '9'=undefined)
			  , Int		-- Kills
			  , Int		-- Ping
			  , String	-- Name
			  )]

deriving instance Ord SockAddr

mastertimeout, polltimeout :: Int
mastertimeout = 300*1000
polltimeout = 400*1000

recvStream :: Socket -> Int -> IO [(String, Int, SockAddr)]
recvStream sock tlimit = do
	start	<- getMicroTime
	let loop = do
		now	<- getMicroTime
		let	wait_	= tlimit - fromInteger (now-start)
			wait	= if wait_ < 0 then 0 else wait_
		test	<- timeout wait $ recvFrom sock 1500
		case test of
			Nothing	-> return []
			Just a	-> (a:) `liftM` loop
	loop


masterGet :: Socket -> SockAddr -> IO [SockAddr]
masterGet sock masterhost = do
	sendTo sock "\xFF\xFF\xFF\xFFgetservers 69 empty full" masterhost
	response <- recvStream sock mastertimeout
	return $ concat [isProper  mess | (mess, _, host) <- response, host == masterhost]
	where isProper x = maybe [] cycleoutIP (shaveOfContainer "\xFF\xFF\xFF\xFFgetserversResponse" "\\EOT\0\0\0" x)

serversGet :: Socket -> ServerMap -> IO ServerMap
serversGet sock themap_ = (loop themap_) `liftM` recvStream sock polltimeout
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
tremulousPollAll host = do
	sock <- socket (addrFamily host) Datagram defaultProtocol
	print "startpoll"
	sp <- getMicroTime
	masterresponse <- masterGet sock (addrAddress host)
	ep <- getMicroTime
	print "master"
	print $ (ep-sp) // 1000
	let servermap = M.fromList [(a, Nothing) | a <- masterresponse]
	polledMaybe <- serversGetResend 3 sock servermap
	ep2 <- getMicroTime
	print "polled"
	print $ (ep2-sp) // 1000
	let (polled, unresponsive) = (M.map (pollFormat . fromJust) $ M.filter isJust polledMaybe, M.size polledMaybe - M.size polled)
	sClose sock
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


pollFormat :: String -> ServerInfo
pollFormat line = (cvars, players) where
		(cvars_:players_)	= splitlines line
		cvars			= cvarstuple . split (=='\\') $ cvars_
		players			= playerList (players_) $ fromMaybe (repeat '9') $ lookup "P" cvars

playerList :: [String] -> String -> PlayerInfo
playerList pa@(p:ps) (l:ls)  =
	case l of
		'-'	->	playerList pa ls
		team	->	( team
				, fromMaybe 0 (mread kills)
				, fromMaybe 1000 (mread ping)
				, fromMaybe "" (mread name)
				) : playerList ps ls
	where	ex (a:b:c:[])	= (a, b, c)
		ex _		= ([], [], [])
		(kills, ping, name) = ex (words p)
playerList _ _ = []

cvarstuple :: [String] -> [(String, String)]
cvarstuple (c:v:ss)	= (c, v) : cvarstuple ss
cvarstuple _		= []
