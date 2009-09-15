module ComTrem (list) where
import Network.Socket
import Text.Printf
import System.IO.Error (try)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Function
import Data.IORef
import Database.HDBC

import CommandInterface
import GeoIP
import TremLib
import TremPolling

data Mode = Small | Full

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

comTremFind _ mess info@Info{echo} = withMasterCache info $ \polled _ ->
	case tremulousFindPlayers polled (split (==',') mess) of
		[] ->
			echo $ "\STX"++mess++":\STX Not found."
		a | atLeastLen (8::Int) a ->
			echo $  "\STX"++mess++":\STX Too many players found, please limit your search."
		a ->
			mapM_ (echo . fixline) a
	where
	fixline (srv, players) = "\STX" ++ srv ++ ":\STX " ++ (ircifyColors $ intercalate "\SI \STX|\STX " players)

comTremServer m _ mess info@Info{echo} state@ComState{geoIP} =  do
	dnsfind			<- resolve mess polldns
	case dnsfind of
		Left _ -> withMasterCache info (\polled _ -> maybe noluck echofunc (tremulousFindServer polled mess)) state
		Right host -> do
			response	<- tremulousPollOne host
			case response of
				Nothing	-> echo $ "\STX"++mess++":\STX No response."
				Just a	-> echofunc (dnsAddress host, a)
	where
	Config {polldns} = config info
	noluck = echo $ "\STX"++mess++":\STX Not found."
	echofunc a  = case m of
		Small	-> echo $ head $ playerLine a geoIP
		Full	-> mapM_ echo $ playerLine a geoIP

comTremClans _ _ info@Info{echo} state@ComState{conn} = withMasterCache info f state where
	f polled _ = do
		clanlist <- map (fromSql . head) `fmap` quickQuery conn "SELECT tag FROM clans" []
		echo $ case tremulousClanList polled clanlist of
			[]	-> "No clans found online."
			str	-> intercalate " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str

comTremStats _ _ info@Info{echo} = withMasterCache info $ \polled time -> do
	now 			<- getMicroTime
	let (tot, ply, bots) = tremulousStats polled
	echo $ printf "%d Servers responded with %d players and %d bots. (cache %ds old)"
		tot ply bots ((now-time)//1000000)

comTremFilter _ mess info@Info{echo} = do
	let (cvar:cmp:value:_) = words mess
	case iscomparefunc cmp of
		False	-> const $ echo $ "\STXcvarfilter:\STX Error in syntax."
		True	-> withMasterCache info $ \polled _ -> do
			let (true, truep, false, falsep, nan, nanp) = tremulousFilter polled cvar cmp value
			echo $ printf "True: %ds %dp \STX|\STX False: %ds %dp \STX|\STX Not found: %ds %dp"
				true truep false falsep nan nanp



resolve :: String -> Map String String -> IO (Either IOError DNSEntry)
resolve servport localdns = try $ getDNS srv port
	where (srv, port) = getIP $ fromMaybe servport (M.lookup (map toLower servport) localdns)


withMasterCache :: Info -> (PollResponse -> Integer -> IO ()) -> ComState -> IO ()
withMasterCache info@Info{echo} f state = do
	poll		<- readIORef p
	pollTime	<- readIORef pT
	host 		<- readIORef pH
	now		<- getMicroTime

	if now-pollTime <= cacheinterval then f poll pollTime else do
		newcache <- try $ tremulousPollAll host
		case newcache of
			Left _		-> echo $ "Error in fetching Master data."
			Right new	-> do
				writeIORef p new
				writeIORef pT now
				f new now
	where	Config {cacheinterval}				= config info
		ComState {poll=p, pollTime=pT, pollHost=pH} 	= state

playerLine :: (SockAddr, ServerInfo) -> GeoIP -> [String]
playerLine (host, ServerInfo cvars players_) geoIP = filter (not . null) final where
	lookSpc a b		= fromMaybe a (lookup (Nocase b) cvars)
	players			= sortBy (flip compare `on` piKills) $ players_
	formatPlayerLine	= intercalate " \STX|\STX " . map (\x -> ircifyColors (piName x) ++" \SI"++show (piKills x)++" ("++show (piPing x)++")")

	(specs, aliens, humans, unknown) = partitionTeams players
	teamx []	= []
	teamx a		= "\STX"++show (piTeam $ head a)++":\STX " ++ formatPlayerLine a ++ "\n"

	summary		= printf "\STXHost:\STX %s \STXName:\STX %s\SI \STXMap:\STX %s \STXPlayers:\STX %s/%s(-%s) \STXØPing:\STX %d \STXCountry:\STX %s"
			(show host) pname pmap pplayers pslots pprivate pping (pcountry host)
	pname		= stripw . take 50 . ircifyColors . filter isPrint $ lookSpc "[noname]" "sv_hostname"
	pmap		= lookSpc "[nomap]" "mapname"
	pplayers	= show . length $ players
	pslots		= lookSpc "?" "sv_maxclients"
	pprivate	= lookSpc "0" "sv_privateclients"
	pping		= intmean . filter (/=999) . map piPing $ players
	pcountry (SockAddrInet _ sip)	= getCountry geoIP (fromIntegral $ flipInt sip)
	pcountry _			= "Unknown" -- This is for IPv6

	final =	[ summary
		, teamx aliens
		, teamx humans
		, teamx specs
		, teamx unknown ]



flipInt :: Word32 -> Word32
flipInt old = new where
	new = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
	a = old .&. 0xFF
	b = (old `shiftR` 8 ) .&. 0xFF
	c = (old `shiftR` 16) .&. 0xFF
	d = (old `shiftR` 24) .&. 0xFF
