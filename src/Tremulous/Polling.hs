module Tremulous.Polling (
	MasterServer(..), Team(..), PlayerInfo(..), ServerInfo(..), PollResponse, CVar
	, emptyPoll, tremulousPollAll, tremulousPollOne
) where
import Network.Socket
import qualified Data.Map as M
import Data.Map (Map)
import Data.Bits
import Text.Read
import System.Timeout
import Control.Exception
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Strategies.DeepSeq
import Data.Foldable
import Control.Monad hiding (mapM_)
import Prelude hiding (all, concat, mapM_, elem)
import Data.Maybe
import Data.List (stripPrefix)
import Helpers

data Team = Spectators | Aliens | Humans| Unknown deriving (Eq, Show)

readTeam :: Char -> Team
readTeam x = case x of
	'0'	-> Spectators
	'1'	-> Aliens
	'2'	-> Humans
	_	-> Unknown

type PollResponse = Map SockAddr ServerInfo
type ServerCache = Map SockAddr (Maybe ServerInfo)
type CVar = (Nocase, String)

data ServerInfo = ServerInfo {
	  cvars		:: ![CVar]
	, players	:: ![PlayerInfo]
	}

data PlayerInfo	= PlayerInfo {
	  team	:: !Team
	, kills
	, ping	:: !Int
	, name	:: !String
	}

data MasterServer = MasterServer {
	  protocol	:: !Int
	, masterHost	:: !SockAddr
	}

instance (Read PlayerInfo) where
	readPrec = do
		Int kills	<- lexP
		Int ping	<- lexP
		String name	<- lexP
		return $ PlayerInfo Unknown (fromInteger kills) (fromInteger ping) name

instance DeepSeq Team

instance DeepSeq ServerInfo where
	deepSeq (ServerInfo c p) = deepSeq c $ deepSeq p

instance DeepSeq PlayerInfo where
	deepSeq x = deepSeq (name x)

deriving instance Ord SockAddr

mastertimeout, polltimeout, singlepolltimeout, resendLimit, minMSrv :: Int
mastertimeout		= 400*1000
polltimeout		= 400*1000
singlepolltimeout	= 800*1000
resendLimit		= 3
minMSrv			= 40

emptyPoll :: PollResponse
emptyPoll = M.empty

-- Spawns a thread (t1) recieving data, package it to Just and send it to a channel.
-- Spawns another thread to wait tlimit micros and then write Nothing to the chan and kill (t1)
-- Left folds over the incoming data.
foldStream :: Socket -> Int -> (v -> (String, Int, SockAddr) -> v) -> v -> IO v
foldStream sock tlimit f v_ = do
	chan	<- atomically newTChan
	tid	<- forkIO $ forever $ (atomically . writeTChan chan . Just) =<< recvFrom sock 1500
	forkIO $ do
		threadDelay tlimit
		killThread tid
		atomically $ writeTChan chan Nothing
	rTChan chan v_
	where rTChan chan !v = do
		cont <- atomically $ readTChan chan
		case cont of
			Nothing -> return v
			Just a	-> rTChan chan (f v a)


masterGet :: Socket -> [MasterServer] -> IO ServerCache
masterGet sock masters = do
	mapM_ (\MasterServer{..} -> sendTo sock (request protocol) masterHost) masters
	foldStream sock mastertimeout (\a n ->  M.union (isProper n) a) M.empty
	where	isProper (x, _, host)
			| host `elem` (fmap masterHost masters) =
				maybe M.empty (nothingKeys . cycleoutIP) (stripPrefix "\xFF\xFF\xFF\xFFgetserversResponse" x)

			| otherwise = M.empty

		nothingKeys = M.fromList . map (flip (,) Nothing)
		request proto = "\xFF\xFF\xFF\xFFgetservers " ++ show proto ++ " empty full"


serversGet :: Socket -> Int -> ServerCache -> IO ServerCache
serversGet sock = f where
	f 0 smap			= return smap
	f _ smap | all isJust smap	= return smap
	f n smap = do
		let noResponse = M.keys $ M.filter isNothing smap
		traverse_ (sendTo sock "\xFF\xFF\xFF\xFFgetstatus") noResponse
		response <- foldStream sock polltimeout getF smap
		f (n-1) response

	getF m (a, _, host) = M.adjust (\_ -> strict `liftM` isProper a) host m
	isProper x = pollFormat =<< stripPrefix "\xFF\xFF\xFF\xFFstatusResponse" x


tremulousPollAll :: [MasterServer] -> IO PollResponse
tremulousPollAll masters = bracket (socket AF_INET Datagram defaultProtocol) sClose $ \sock -> do
	masterresponse <- notZero resendLimit $ masterGet sock masters
	(strict . M.mapMaybe id) `liftM` serversGet sock resendLimit masterresponse
	where
	notZero times action = f times M.empty where
		f 0 m = return m
		f n m = do
			new <- (`M.union` m) `fmap` action
			if M.size new >= minMSrv
				then return new
				else f (n-1) new


tremulousPollOne :: DNSEntry -> IO (Maybe ServerInfo)
tremulousPollOne DNSEntry{dnsAddress, dnsFamily} = bracket (socket dnsFamily Datagram defaultProtocol) sClose $ \sock -> do
	connect sock dnsAddress
	send sock "\xFF\xFF\xFF\xFFgetstatus"
	poll <- timeout singlepolltimeout $ recv sock 1500
	return $ pollFormat =<< isProper =<< poll
	where isProper = stripPrefix "\xFF\xFF\xFF\xFFstatusResponse"

cycleoutIP :: String -> [SockAddr]
cycleoutIP ('\\' :'E':'O':'T':'\0':'\0':'\0':[]) = []
cycleoutIP ('\\' : i0:i1:i2:i3 : p0:p1 : xs) = SockAddrInet port ip : cycleoutIP xs
	where	ip	= fromIntegral $ (ord i3 .<<. 24) .|. (ord i2 .<<. 16) .|. (ord i1 .<<. 8) .|. ord i0
		port	= fromIntegral $ (ord p0 .<<. 8) .|. ord p1
		(.<<.)	= shiftL
cycleoutIP _ = []

pollFormat :: String -> Maybe ServerInfo
pollFormat line = case splitlines line of
		(cvars_:players_) -> let
			cvars	= cvarstuple . split (=='\\') $ cvars_
			players	= maybe (mapMaybe mread players_) (playerList players_) (lookup (Nocase "P") cvars)
			in Just $ ServerInfo cvars players
		_ -> Nothing

playerList :: [String] -> [Char] -> [PlayerInfo]
playerList []		_	= []
playerList (p:ps)	[]	= mread p /: playerList ps []
playerList ps		('-':ls)= playerList ps ls
playerList (p:ps)	(l:ls)	= (\x -> x {team = readTeam l}) `liftM` mread p /: playerList ps ls

cvarstuple :: [String] -> [CVar]
cvarstuple (c:v:ss)	= (Nocase c, v) : cvarstuple ss
cvarstuple _		= []

(/:) :: Maybe t -> [t] -> [t]
(Just x) /: xs = x:xs
Nothing /: xs = xs