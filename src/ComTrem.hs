module ComTrem (m) where
import Network.Socket
import Text.Printf
import System.IO.Error (try)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Function

import CommandInterface hiding (name)
import GeoIP
import TremLib
import TremPolling

data Mode = Small | Full

m :: Module
m = Module
	{ modName	= "tremulous"
	, modInit	= tremInit
	, modFinish	= modifyCom $ \x -> x { trempoll = HolderTrem 0 (getHost $ trempoll x) emptyPoll }
	, modList	= list
	}
	where getHost (HolderTrem _ x _) = x

tremInit :: River CState ()
tremInit = do
	path	<- gets configPath
	master	<- gets (masterserver . config)
	host	<- io $ uncurry getDNS (getIP master)
	geoIP	<- io $ fromFile $ path ++ "IpToCountry.csv"
	modifyCom $ \x -> x
		{ trempoll = HolderTrem 0 host emptyPoll
		, geoIP
		}

list :: CommandList
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

comTremFind, comTremStats, comTremClans, comTremFilter :: Command
comTremServer :: Mode -> Command

comTremFind mess' = withMasterCache $ \polled _ ->
	case tremulousFindPlayers polled search of
		[] ->
			dsp "Not found."
		a | atLeastLen (8::Int) a ->
			dsp "Too many players found, please limit your search."
		a ->
			EchoM >>> fmap fixline a
	where
	fixline (srv, players) = view srv (ircifyColors $ intercalate "\SI \STX|\STX " players)
	search = split (==',') mess'
	dsp x = case search of
		[a]	-> Echo >>> view a x
		_	-> Error >>> x

comTremServer mode mess = do
	dnsfind	<- resolve mess
	case dnsfind of
		Left _ -> withMasterCache $ \polled _ -> case tremulousFindServer polled mess of
			Nothing	-> Echo >>> view mess "Not found."
			Just a	-> echofunc a
		Right host -> do
			response <- io $ tremulousPollOne host
			case response of
				Nothing	-> Echo >>> view mess "No response."
				Just a	-> echofunc (dnsAddress host, a)
	where
	echofunc a@(_, ServerInfo _ players) = do
		geoIP	<- getsCom geoIP
		case mode of
			Small	-> Echo >>> serverSummary a geoIP
			Full	-> EchoM >>> (serverSummary a geoIP : serverPlayers players)

comTremClans _ = withMasterCache $ \ polled _ -> do
	clanlist <- map (fromSql . head) `fmap` sqlQuery "SELECT tag FROM clans" []
	Echo >>> case tremulousClanList polled clanlist of
		[]	-> "No clans found online."
		str	-> intercalate " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str

comTremStats _ = withMasterCache $ \polled time -> do
	now <- io getMicroTime
	let (tot, ply, bots) = tremulousStats polled
	Echo >>> printf "%d Servers responded with %d players and %d bots. (cache %ds old)"
		tot ply bots ((now-time)//1000000)

comTremFilter mess = do
	let (cvar:cmp:value:_) = words mess
	case iscomparefunc cmp of
		False	-> Error >>> "Error in syntax."
		True	-> withMasterCache $ \polled _ -> do
			let (true, truep, false, falsep, nan, nanp) = tremulousFilter polled cvar cmp value
			Echo >>> printf "True: %ds %dp \STX|\STX False: %ds %dp \STX|\STX Not found: %ds %dp"
				true truep false falsep nan nanp



resolve :: String -> RiverCom (Either IOError DNSEntry)
resolve servport = do
	polldns <- gets (polldns . config)
	let (srv, port) = getIP $ fromMaybe servport (M.lookup (Nocase servport) polldns)
	io $ try $ getDNS srv port


withMasterCache :: (PollResponse -> Integer -> RiverCom ()) -> RiverCom ()
withMasterCache f = do
	HolderTrem pollTime host poll <- getsCom trempoll
	now		<- io getMicroTime
	interval	<- gets (cacheinterval . config)
	protocol	<- gets (masterprotocol . config)

	if now-pollTime <= interval then f poll pollTime else do
		newcache <- io $ try $ tremulousPollAll host protocol
		case newcache of
			Left e		-> do
				Error >>> "Error fetching the masterserver"
				trace $ show e
			Right new	-> do
				modifyCom $ \x -> x {trempoll=HolderTrem now host new}
				f new now

serverSummary :: (SockAddr, ServerInfo) -> GeoIP -> String
serverSummary (host, ServerInfo cvars players) geoIP = unwords $ fmap (uncurry view)
	[ ("Host"	, show host)
	, ("Name"	, (sanitize $ look "[noname]" "sv_hostname") ++ "\SI")
	, ("Map"	, look "[nomap]" "mapname")
	, ("Players"	, (show $ length players) ++ "/" ++ look "?" "sv_maxclients" ++ "(-"++look "0" "sv_privateclients"++")")
	, ("ØPing"	, show $ intmean . filter (/=999) . map ping $ players)
	, ("Country"	, ipLocate host)
	]
	where
		look a b	= fromMaybe a (lookup (Nocase b) cvars)
		sanitize	= ircifyColors . stripw . take 50 . filter (\x -> let a = ord x in a >= 32 && a <= 127)
		ipLocate (SockAddrInet _ sip)	= getCountry geoIP (fromIntegral $ flipInt sip)
		ipLocate _			= "Unknown" -- For IPv6

serverPlayers :: [PlayerInfo] -> [String]
serverPlayers players'' = let
	players		= sortBy (flip compare `on` kills) $ players''
	(specs, aliens, humans, unknown) = partitionTeams players
	in fmap teamFormat $ filter (not . null) [aliens, humans, specs, unknown]
	where
	teamFormat xs	= view (show (team $ head xs)) $ playersFormat xs
	playersFormat 	= intercalate " \STX|\STX " . map (\(PlayerInfo _ kills ping name) -> ircifyColors name ++ "\SI " ++ show kills ++ " " ++ show ping)

flipInt :: Word32 -> Word32
flipInt old = new where
	new = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
	a = old .&. 0xFF
	b = (old `shiftR` 8 ) .&. 0xFF
	c = (old `shiftR` 16) .&. 0xFF
	d = (old `shiftR` 24) .&. 0xFF
