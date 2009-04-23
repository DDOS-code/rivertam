module ComTrem (list) where
import Network.Socket
import qualified Data.Map as M
import Text.Printf
import Data.Bits
import Data.Word
import System.IO.Error (try)

import qualified GeoIP
import TremLib
import TremMasterCache
import Send
import RiverState
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
	]

comTremFind, comTremStats, comTremClans :: Command
comTremServer :: Mode -> Command

comTremFind (_, chan, mess) = withMasterCache chan $ do
	polled <- gets rivCache
	case tremulousFindSimple polled mess of
		[] ->
			Msg chan >>> "\STX"++mess++"\STX: Not found."
		a | length a > 5 ->
			Msg chan >>> "\STX"++mess++"\STX: Too many players found, please limit your search."
		a ->
			mapM_ (Msg chan >>>) $ map fixline a

	where fixline (srv,players) = printf "\STX%s\SI [\STX%d\STX]: %s"
		(stripw . removeColors $ srv) (length players) (ircifyColors $ unsplit "\SI \STX|\STX " players)

comTremServer m (_, chan, mess) = withMasterCache chan $ do
	rivCache <- gets rivCache
	Config {polldns} <- gets config
	rivGeoIP <- gets rivGeoIP
	poll <- lift $ resolve mess polldns

	let	noluck = Msg chan >>> "\STX"++mess++"\STX: Not found."
		echofunc a  = case m of
			Small	-> Msg chan >>> head $ playerLine a rivGeoIP
			Full	-> mapM_ (Msg chan >>>) $ playerLine a rivGeoIP
	case poll of
		Left _ -> maybe noluck echofunc (tremulousFindServer rivCache mess)
		Right aa -> maybe noluck (\x -> echofunc (aa, x)) (M.lookup aa (fst rivCache))


comTremClans (_, chan, _) = withMasterCache chan $ do
	rivCache <- gets rivCache
	Config {clanlist} <- gets config
	case tremulousClanList rivCache clanlist of
		[]	-> Msg chan >>> "No clans found online."
		str	-> Msg chan >>> unsplit " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str

comTremStats (_, chan, _) = withMasterCache chan $ do
	rivCache	<- gets rivCache
	rivCacheTime	<- gets rivCacheTime
	now 		<- lift $ getMicroTime
	let (ans, tot, ply) = tremulousStats rivCache
	Msg chan >>> printf "%d/%d Servers responded with %d players. (cache %ds old)"
		ans tot ply ((now-rivCacheTime)//1000000)


getIP :: String -> (String, String)
getIP str = case break (==':') str of
		(a, [])	-> (a, "30720")
		(a, b)	-> (a, drop 1 b)

resolve :: String -> Map String String -> IO (Either IOError SockAddr)
resolve servport localdns = try $ (addrAddress . head) `liftM` getAddrInfo Nothing (Just srv) (Just port)
	where (srv, port) = getIP $ fromMaybe servport (M.lookup (map toLower servport) localdns)

withMasterCache :: String -> RiverState -> RiverState
withMasterCache chan f = do
	success	<- masterReCache
	if success then f else Msg chan >>> "Error in fetching Master data."

masterReCache ::  StateT River IO Bool
masterReCache = do
	rivCacheTime <- gets rivCacheTime
	Config {cacheinterval} <- gets config
	now <- lift $ getMicroTime
	if now-rivCacheTime > cacheinterval then do
		newcache <- lift $ try $ tremulousPollAll
		case newcache of
			Left _ | rivCacheTime == 0 ->
				return False
			Left _ ->
				return True
			Right new -> do
				modify (\x -> x {rivCacheTime=now, rivCache=new})
				return True
		else return True

playerLine :: (SockAddr, ServerInfo) -> GeoIP.Data -> [String]
playerLine (host, (cvars,players_)) geoIP = filter (not . null) echo where
	lookSpc a b = fromMaybe a (M.lookup b cvars)
	players = sortBy (\(_,a1,_,_) (_,a2,_,_) -> compare a2 a1) $ players_
	avgping = if length players == 0 then 0 else (sum [a | (_,_,a,_) <- players, a /= 999]) // (length players)
	teamfilter filt = unsplit " \STX|\STX " [ircifyColors name++" \SI"++show kills | (team, kills, _, name) <- players, team == filt]
	teamx team filt = case teamfilter filt of
		[]	-> []
		a	-> "\STX"++team++":\STX " ++ a ++ "\n"

	line		= printf "\STXHost:\STX %s \STXName:\STX %s\SI \STXMap:\STX %s \STXPlayers:\STX %s/%s(-%s) \STXøPing:\STX %d \STXCountry:\STX %s"
			(show host) pname pmap pplayers pslots pprivate avgping (pcountry host)
	pname		= stripw . take 50 . ircifyColors . filter isPrint $ lookSpc "[noname]" "sv_hostname"
	pmap		= lookSpc "[nomap]" "mapname"
	pplayers	= show . length . filter (/='-') $ (lookSpc "-" "P")
	pslots		= lookSpc "?" "sv_maxclients"
	pprivate	= lookSpc "0" "sv_privateClients"
	pcountry (SockAddrInet _ sip)	= GeoIP.getCountry geoIP (fromIntegral $ flipInt sip)
	pcountry _			= "Unknown" -- This is for IPv6 countries

	echo =	[ line
		, teamx "Aliens" '1'
		, teamx "Humans" '2'
		, teamx "Spectators" '0' ]

flipInt :: Word32 -> Word32
flipInt old = new where
	new = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
	a = old .&. 0xFF
	b = (old `shiftR` 8 ) .&. 0xFF
	c = (old `shiftR` 16) .&. 0xFF
	d = (old `shiftR` 24) .&. 0xFF
