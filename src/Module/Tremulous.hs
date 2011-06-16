module Module.Tremulous (mdl) where
import Network.Socket
import Text.Printf
import System.IO.Error (try)
import qualified Data.Map as M
import Data.List
import Data.Maybe
--import Data.Bits
--import Data.Word
import Data.Function

import Module hiding (name)
import Module.State
import Module.RiverHDBC
import Tremulous.Protocol
import Tremulous.Polling
import Tremulous.Util

data Mode = Small | Full

mdl :: Module State
mdl = Module
	{ modName	= "tremulous"
	, modInit	= tremInit
	, modFinish	= modifyCom $ \x -> x { trempoll = HolderTrem 0 (getHost $ trempoll x) [] }
	, modList	= list
	}
	where getHost (HolderTrem _ x _) = x

tremInit :: River State ()
tremInit = do
	masterserver	<- gets (masterserver . config)
	host		<- io $ mapM (\(s, h, p) -> MasterServer s p <$> dnsAddress <$> uncurry getDNS (getIP h)) masterserver
	--geoIP	<- io $ fromFile $ path ++ "IpToCountry.csv"
	modifyCom $ \x -> x
		{ trempoll = HolderTrem 0 host []
		--, geoIP
		}

list :: CommandList State
list =
	[ ("find"		, (comTremFind		, 1	, Peon	, "<<player>>"
		, "Find tremulous players. Separate multiple players with comma."))
	, ("poll"		, (comTremServer Small	, 1	, Peon	, "<<server>>"
		, "Brief info about a tremulous server. Search either on the hostname or enter an url."))
	, ("listplayers"	, (comTremServer Full	, 1	, Peon	, "<<server>>"
		, "List the players on a tremulous server. Search either on the hostname or enter an url."))
	, ("onlineclans"	, (comTremClans		, 0	, Peon	, ""
		, "List all online tremulous clans."))
	, ("tremstats"		, (comTremStats		, 0	, Peon	, ""
		, "Statistics about all tremulous servers."))
	, ("cvarfilter"		, (comTremFilter	, 3	, Peon	, "<cvar> <cmpfunc> <int/string>"
		, "Generate statistics for a specific cvar. Example: cvarfilter g_unlagged >= 1. Allowed functions: == | /= | > | >= | < | <="))
	]

comTremFind, comTremStats, comTremFilter, comTremClans :: Command State
comTremServer :: Mode -> Command State

comTremFind = withMasterCache $ \polled _ mess' -> let
	fixline (srv, players) = view srv (ircifyColors $ intercalate "\SI \STX|\STX " players)
	search = split (==',') mess'
	dsp x = case search of
		[a]	-> Echo >>> view a x
		_	-> Error >>> x

	in case findPlayers polled search of
		[] ->
			dsp "Not found."
		a | atLeastLen (8::Int) a ->
			dsp "Too many players found, please limit your search."
		a ->
			EchoM >>> fmap fixline a

comTremServer mode mess_ = do
	dnsfind	<- resolve mess_
	case dnsfind of
		Left _ -> withMasterCache  (\polled _ mess -> case findServer polled mess of
			Nothing	-> Echo >>> view mess "Not found."
			Just a	-> echofunc a) mess_
		Right host -> do
			response <- io $ pollOne host
			case response of
				Nothing	-> Echo >>> view mess_ "No response."
				Just a	-> echofunc a
	where
	echofunc a@GameServer{players} = do
		--geoIP	<- getsCom geoIP
		case mode of
			Small	-> Echo >>> serverSummary a --geoIP
			Full	-> EchoM >>> (serverSummary a {-geoIP-} : serverPlayers players)

comTremClans = withMasterCache $ \ polled _ _ -> do
	clanlist <- map (fromSql . head) `fmap` sqlQuery "SELECT tag FROM clans" []
	Echo >>> case clanList polled clanlist of
		[]	-> "No clans found online."
		str	-> intercalate " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str


comTremStats = withMasterCache $ \polled time _ -> do
	now <- io getMicroTime
	let (tot, ply, bots) = stats polled
	Echo >>> printf "%d Servers responded with %d players and %d bots. (cache %ds old)"
		tot ply bots ((now-time)//1000000)

comTremFilter = withMasterCache $ \polled _ mess -> do
	let (cvar:cmp:value:_) = words mess
	case iscomparefunc cmp of
		False	-> Error >>> "Error in syntax."
		True	-> do
			let (true, truep, false, falsep, nan, nanp) = tremulousFilter polled cvar cmp value
			Echo >>> printf "True: %ds %dp \STX|\STX False: %ds %dp \STX|\STX Not found: %ds %dp"
				true truep false falsep nan nanp



resolve :: String -> RiverCom State (Either IOError DNSEntry)
resolve servport = do
	let (srv, port) = getIP servport
	io $ try $ getDNS srv port


withMasterCache :: ([GameServer] -> Integer -> String -> RiverCom State ()) -> String -> RiverCom State ()
withMasterCache f str = do
	HolderTrem pollTime masters poll <- getsCom trempoll
	now		<- io getMicroTime
	interval	<- gets (cacheinterval . config)
	let 	(mfilt, strnew) = findWords str
		ff p = case string2proto =<< mfilt of
			Just a	-> filter (\y -> gameproto y == a) p
			Nothing	-> p

	if now-pollTime <= interval then f (ff poll) pollTime strnew else do
		newcache <- io $ try $ pollMasters masters
		case newcache of
			Left e		-> do
				Error >>> "Error fetching the masterserver."
				trace $ show e
			Right new	-> do
				modifyCom $ \x -> x {trempoll=HolderTrem now masters new}
				f (ff new) now strnew

-- For finding %lala in a string
findWords :: String -> (Maybe String, String)
findWords = g . partition f . words
	where
	g ([], x)	= (Nothing, unwords x)
	g (x:_, y)	= (Just (drop 1 x), unwords y)

	f ('%':_:_)	= True
	f _ 		= False


serverSummary :: GameServer ->  String
serverSummary GameServer{..} = 
	"\STX[\STX" ++ (proto2string gameproto) ++ "\STX]\STX " ++
	(unwords $ fmap (uncurry view)
	([ ("Host"	, show address)
	, ("Name"	, sanitize hostname)
	, ("Map"	, mapname)
	, ("Players"	, (show numplayers) ++ "/" ++ show slots ++ "(+"++ show privslots++")")
	] ++ if numplayers > 0 
		then [("ØPing"	, show $ intmean . filter validping . map ping $ players)]
		else [])
	)
	where
	numplayers	= length players
	look a b	= fromMaybe a (lookup (Nocase b) cvars)
	sanitize	= ircifyColors . stripw . take 50 . filter (\x -> let a = ord x in a >= 32 && a <= 127)
	validping x	= x > 0 && x < 999
	--ipLocate (SockAddrInet _ sip)	= getCountry geoIP (fromIntegral $ flipInt sip)
	--ipLocate _			= "Unknown" -- For IPv6

serverPlayers :: [PlayerInfo] -> [String]
serverPlayers players'' = let
	players		= sortBy (flip compare `on` kills) $ players''
	(specs, aliens, humans, unknown) = partitionTeams players
	in fmap teamFormat $ filter (not . null) [aliens, humans, specs, unknown]
	where
	teamFormat xs	= view (show (team $ head xs)) $ playersFormat xs
	playersFormat 	= intercalate " \STX|\STX " . map (\(PlayerInfo _ kills ping name) -> ircifyColors name ++ "\SI " ++ show kills ++ " " ++ show ping)

{-
flipInt :: Word32 -> Word32
flipInt old = new where
	new = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
	a = old .&. 0xFF
	b = (old `shiftR` 8 ) .&. 0xFF
	c = (old `shiftR` 16) .&. 0xFF
	d = (old `shiftR` 24) .&. 0xFF
-}
