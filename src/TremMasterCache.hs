module TremMasterCache (
	  Team(..)
	, ServerCache
	, ServerInfo
	, PlayerInfo
	, tremulousPollAll
) where

import Network.Socket
import qualified Data.Map as M
import Data.Map(Map)
import Data.Bits
import System.IO
import System.IO.Unsafe
import Control.Monad
import Control.Exception (bracket)
import Control.Parallel.Strategies
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Helpers

data Team = Spectators | Aliens | Humans| Unknown | UnusedSlot deriving (Eq, Show)

readTeam :: Char -> Team
readTeam x = case x of
	'0'	-> Spectators
	'1'	-> Aliens
	'2'	-> Humans
	'-'	-> UnusedSlot
	_	-> Unknown

type ServerMap = Map SockAddr (Maybe String)
type ServerCache = (Map SockAddr ServerInfo, Int)
type ServerInfo = ([(String, String)], [PlayerInfo])
type PlayerInfo		= (Team		-- Team
			  , Int		-- Kills
			  , Int		-- Ping
			  , String	-- Name
			  )

deriving instance Ord SockAddr

mastertimeout, polltimeout :: Int
mastertimeout = 300*1000
polltimeout = 400*1000

{-
recvStream :: Socket -> Int -> IO [(String, Int, SockAddr)]
recvStream sock tlimit = do
	start	<- getMicroTime
	let loop = do
		now	<- getMicroTime
		let	wait	= tlimit - fromInteger (now-start)
		test	<- if wait < 1 then return Nothing else timeout wait $ recvFrom sock 1500
		case test of
			Nothing	-> return []
			Just a	-> unsafeInterleaveIO $ (a:) `liftM` loop
	loop
-}
-- Spawns a thread (t1) recieving data, package it to Just and send it to a channel.
-- Spawns another thread to wait tlimit micros and then write Nothing to the chan and kill (t1)
-- Return a function that reads the channel until it reaches nothing
recvStream :: Socket -> Int -> IO [(String, Int, SockAddr)]
recvStream sock tlimit = do
		chan	<- atomically newTChan
		tid	<- forkIO $ forever $ (atomically . writeTChan chan . Just) =<< recvFrom sock 1500
		forkIO $ do
			threadDelay tlimit
			killThread tid
			atomically $ writeTChan chan Nothing

		lazyTChan chan

		where lazyTChan chan = do
			cont <- atomically $ readTChan chan
			case cont of
				Nothing -> return []
				Just a	-> unsafeInterleaveIO $ liftM (a:) (lazyTChan chan)


masterGet :: Socket -> SockAddr -> IO [SockAddr]
masterGet sock masterhost = do
	sendTo sock "\xFF\xFF\xFF\xFFgetservers 69 empty full" masterhost
	(forceval seqList . streamToIp) =^! recvStream sock mastertimeout
	where	streamToIp x	= concat [isProper mess | (mess, _, host) <- x, host == masterhost]
		isProper x	= maybe [] cycleoutIP (shaveOfContainer "\xFF\xFF\xFF\xFFgetserversResponse" "\\EOT\0\0\0" x)

serversGet :: Socket -> ServerMap -> IO ServerMap
serversGet sock themap_ = (loop themap_) `liftM` recvStream sock polltimeout
	where	loop themap [] = themap
		loop themap ((a, _, host):xs) = case M.lookup host themap of
			Just _ -> loop (M.insert host (isProper a) themap) xs
			Nothing -> loop themap xs
		isProper = shavePrefix "\xFF\xFF\xFF\xFFstatusResponse"


serversGetResend ::	Int ->	Socket -> ServerMap -> IO ServerMap
serversGetResend 	0	_	!servermap	= return servermap
serversGetResend 	_	_	!servermap
	| (M.size $ M.filter isNothing servermap) == 0 	= return servermap
serversGetResend 	!n	sock	!servermap 	= do
	let (sJust, sNothing) = (M.filter isJust servermap, M.filter isNothing servermap)
	mapM_ (sendTo sock "\xFF\xFF\xFF\xFFgetstatus") [a | (a, _) <- M.toList sNothing]
	response <- serversGet sock servermap
	serversGetResend (n-1) sock (M.unionWith priojust response sJust)
	where
		priojust	(Just a)	Nothing		= Just a
		priojust	Nothing		(Just a)	= Just a
		priojust	(Just a)	(Just _)	= Just a
		priojust	Nothing		Nothing		= Nothing


tremulousPollAll :: DNSEntry -> IO ServerCache
tremulousPollAll host = bracket (socket (dnsFamily host) Datagram defaultProtocol) sClose $ \sock -> do
	masterresponse <- masterGet sock (dnsAddress host)
	let servermap = M.fromList [(a, Nothing) | a <- masterresponse]
	polledMaybe <- serversGetResend 3 sock servermap
	let	!polled		= M.map (pollFormat . fromJust) $ M.filter isJust polledMaybe
		!unresponsive 	= M.size polledMaybe - M.size polled
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
		players			= playerList (players_) (fromMaybe (repeat Unknown) $ map readTeam `liftM` lookup "p" cvars)

playerList :: [String] -> [Team] -> [PlayerInfo]
playerList pa@(p:ps) (l:ls)  =
	case l of
		UnusedSlot	-> playerList pa ls
		team		-> maybe (playerList ps ls) (:playerList ps ls) (pstring team p)
playerList _ _ = []

pstring :: Team -> String -> Maybe PlayerInfo
pstring team str = do
	ks	<- mread kills
	pg	<- mread ping
	nm	<- mread name
	return (team, ks, pg, nm)
	where	(kills, buf)	= break isSpace str
		(ping, buf2)	= break isSpace $ dropWhile isSpace buf
		name		= dropWhile isSpace buf2

cvarstuple :: [String] -> [(String, String)]
cvarstuple (c:v:ss)	= (map toLower c, v) : cvarstuple ss
cvarstuple _		= []
