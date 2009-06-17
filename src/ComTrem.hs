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
import Data.IORef


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
		, "Find a tremulous players. Separate with comma."))
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

comTremFind _ mess info@Info{echo} = withMasterCache info $ \polled _ -> do
	case tremulousFindPlayers polled args of
		[] ->
			echo $ "\STX"++mess++"\STX: Not found."
		a | length a > 7 ->
			echo $  "\STX"++mess++"\STX: Too many players found, please limit your search."
		a ->
			mapM_ echo $ map fixline a

	where
	fixline (srv,players) = printf "\STX%s\SI: %s"
		(stripw . removeColors $ srv) (ircifyColors $ intercalate "\SI \STX|\STX " players)
	args = map stripw $ split (==',') mess

comTremServer m _ mess info@Info{echo} state@ComState{geoIP} =  do
	dnsfind			<- resolve mess polldns
	case dnsfind of
		Left _ -> withMasterCache info (\polled _ -> maybe noluck echofunc (tremulousFindServer polled mess)) state
		Right host -> do
			response	<- tremulousPollOne host
			case response of
				Nothing	-> echo $  "\STX"++mess++"\STX: No response."
				Just a	-> echofunc (dnsAddress host, a)
	where
	Config {polldns} = config info
	noluck = echo $ "\STX"++mess++"\STX: Not found."
	echofunc a  = case m of
		Small	-> echo $ head $ playerLine a geoIP
		Full	-> mapM_ echo $ playerLine a geoIP

comTremClans _ _ info@Info{echo} = withMasterCache info $ \polled _ -> do
	case tremulousClanList polled clanlist of
		[]	-> echo $  "No clans found online."
		str	-> echo $  intercalate " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str
	where Config {clanlist} = config info

comTremStats _ _ info@Info{echo} = withMasterCache info $ \polled time -> do
	now 			<- getMicroTime
	let (ans, tot, ply, bots) = tremulousStats polled
	echo $ printf "%d/%d Servers responded with %d players and %d bots. (cache %ds old)"
		ans tot ply bots ((now-time)//1000000)

comTremFilter _ mess info@Info{echo} state = do
	let	(cvar_:cmp:value:_)	= words mess
		cvar			= map toLower cvar_

	case iscomparefunc cmp of
		False	-> echo $ "\STXcvarfilter:\STX Error in syntax."
		True	-> case mread value :: Maybe Int of
				Nothing -> doit ((comparefunc cmp) value) state
				Just intvalue -> doit (intcmp (comparefunc cmp)  intvalue) state
			where doit func = withMasterCache info $ \polled _ -> do
				let (true, truep, false, falsep, nan, nanp) = tremulousFilter polled cvar func
				echo $ printf "True: %ds %dp \STX|\STX False: %ds %dp \STX|\STX Not found: %ds %dp"
							true truep false falsep nan nanp

	where intcmp f val x = case mread x :: Maybe Int of
		Nothing -> False
		Just a	-> a `f` val


resolve :: String -> Map String String -> IO (Either IOError DNSEntry)
resolve servport localdns = try $ getDNS srv port
	where (srv, port) = getIP $ fromMaybe servport (M.lookup (map toLower servport) localdns)


withMasterCache :: Info -> (ServerCache -> Integer -> IO ()) -> ComState -> IO ()
withMasterCache info@Info{echo} f state = do
	poll		<- readIORef p
	pollTime	<- readIORef pT
	host 		<- readIORef pH
	now		<- getMicroTime

	if now-pollTime <= cacheinterval then f poll pollTime else do
		newcache <- try $ tremulousPollAll host
		case newcache of
			Left _		-> echo $  "Error in fetching Master data."
			Right new	-> do
				writeIORef p new
				writeIORef pT now
				f new now
	where	Config {cacheinterval}				= config info
		ComState {poll=p, pollTime=pT, pollHost=pH} 	= state

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
