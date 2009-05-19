module ComTrem (list) where
import Network.Socket
import qualified Data.Map as M
import Text.Printf
import Data.Bits
import Data.Word
import System.IO.Error (try)

import CommandInterface
import GeoIP
import TremLib
import TremMasterCache
import Config
import Helpers

data Mode = Small | Full

list :: CommandList
list =
	[ ("find"		, (comTremFind		, 1	, Peon	, "<<player>>"
		, "Find a tremulous player."))
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

comTremFind _ mess info = withMasterCache info $ \(polled,_) -> do
	case tremulousFindSimple polled mess of
		[] ->
			echo . Mess $ "\STX"++mess++"\STX: Not found."
		a | length a > 5 ->
			echo . Mess $  "\STX"++mess++"\STX: Too many players found, please limit your search."
		a ->
			mapM_ (echo . Mess) $ map fixline a

	where fixline (srv,players) = printf "\STX%s\SI [\STX%d\STX]: %s"
		(stripw . removeColors $ srv) (length players) (ircifyColors $ intercalate "\SI \STX|\STX " players)

comTremServer m _ mess info =  do
	rivGeoIP 		<- gets geoIP
	dnsfind			<- lift $ resolve mess polldns

	let	noluck = echo . Mess $  "\STX"++mess++"\STX: Not found."
		echofunc a  = case m of
			Small	-> echo . Mess $  head $ playerLine a rivGeoIP
			Full	-> mapM_ (echo . Mess $ ) $ playerLine a rivGeoIP
	case dnsfind of
		Left _ -> withMasterCache info $ \(polled,_) -> maybe noluck echofunc (tremulousFindServer polled mess)
		Right host -> do
			response	<- lift $ tremulousPollOne host
			case response of
				Nothing	-> echo . Mess $  "\STX"++mess++"\STX: No response."
				Just a	-> echofunc (dnsAddress host, a)
	where Config {polldns} = config2 info


comTremClans _ _ info = withMasterCache info $ \(polled,_) -> do
	case tremulousClanList polled clanlist of
		[]	-> echo . Mess $  "No clans found online."
		str	-> echo . Mess $  intercalate " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str
	where Config {clanlist} = config2 info

comTremStats _ _ info = withMasterCache info $  \(polled,time) -> do
	now 			<- lift $ getMicroTime
	let (ans, tot, ply, bots) = tremulousStats polled
	echo . Mess $  printf "%d/%d Servers responded with %d players and %d bots. (cache %ds old)"
		ans tot ply bots ((now-time)//1000000)

comTremFilter _ mess info = do
	let	(cvar_:cmp:value:_)	= words mess
		cvar			= map toLower cvar_

	case iscomparefunc cmp of
		False	-> echo . Mess $  "\STXcvarfilter:\STX Error in syntax."
		True	-> case mread value :: Maybe Int of
				Nothing -> doit $ (comparefunc cmp) value
				Just intvalue -> doit (intcmp (comparefunc cmp)  intvalue)
			where doit func = withMasterCache info $ \(polled,_) -> do
				let (true, truep, false, falsep, nan, nanp) = tremulousFilter polled cvar func
				echo . Mess $  printf "True: %ds %dp \STX|\STX False: %ds %dp \STX|\STX Not found: %ds %dp"
							true truep false falsep nan nanp

	where intcmp f val x = case mread x :: Maybe Int of
		Nothing -> False
		Just a	-> a `f` val


resolve :: String -> Map String String -> IO (Either IOError DNSEntry)
resolve servport localdns = try $ getDNS srv port
	where (srv, port) = getIP $ fromMaybe servport (M.lookup (map toLower servport) localdns)


withMasterCache :: Info -> ((ServerCache, Integer) -> Transformer ()) -> Transformer ()
withMasterCache info f = do
	poll		<- gets poll
	pollTime	<- gets pollTime
	host 		<- gets pollHost
	now		<- lift $ getMicroTime

	if now-pollTime <= cacheinterval then f (poll, pollTime) else do
		newcache <- lift $ try $ tremulousPollAll host
		case newcache of
			Left _		-> echo . Mess $  "Error in fetching Master data."
			Right new	-> do
				modify $ \x -> x {
					  poll		= new
					, pollTime	= now
					}
				f (new, now)
	where Config {cacheinterval}	= config2 info

playerLine :: (SockAddr, ServerInfo) -> GeoIP -> [String]
playerLine (host, ServerInfo cvars players_) geoIP = filter (not . null) final where
	lookSpc a b		= fromMaybe a (lookup b cvars)
	players			= sortBy (\a b -> compare (piKills b) (piKills a)) $ players_
	formatPlayerLine	= intercalate " \STX|\STX " . map (\x -> ircifyColors (piName x) ++" \SI"++show (piKills x)++" ("++show (piPing x)++")")


	teamx filt = case filter (\x -> piTeam x == filt) players  of
		[]	-> []
		a	-> "\STX"++show filt++":\STX " ++ formatPlayerLine a ++ "\n"

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
		, teamx Aliens
		, teamx Humans
		, teamx Spectators
		, teamx Unknown ]



flipInt :: Word32 -> Word32
flipInt old = new where
	new = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
	a = old .&. 0xFF
	b = (old `shiftR` 8 ) .&. 0xFF
	c = (old `shiftR` 16) .&. 0xFF
	d = (old `shiftR` 24) .&. 0xFF
