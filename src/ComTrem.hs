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

mastersrv, masterport :: String
mastersrv =  "master.tremulous.net"
masterport = "30710"

comTremFind, comTremStats, comTremClans :: Command
comTremServer :: Mode -> Command

comTremFind (_, chan, mess) = withMasterCache chan $ \(polled,_) -> do
	case tremulousFindSimple polled mess of
		[] ->
			Msg chan >>> "\STX"++mess++"\STX: Not found."
		a | length a > 5 ->
			Msg chan >>> "\STX"++mess++"\STX: Too many players found, please limit your search."
		a ->
			mapM_ (Msg chan >>>) $ map fixline a

	where fixline (srv,players) = printf "\STX%s\SI [\STX%d\STX]: %s"
		(stripw . removeColors $ srv) (length players) (ircifyColors $ intercalate "\SI \STX|\STX " players)

comTremServer m (_, chan, mess) = withMasterCache chan $ \(polled,_) -> do
	Config {polldns}	<- gets config
	rivGeoIP 		<- gets rivGeoIP
	poll <- lift $ resolve mess polldns

	let	noluck = Msg chan >>> "\STX"++mess++"\STX: Not found."
		echofunc a  = case m of
			Small	-> Msg chan >>> head $ playerLine a rivGeoIP
			Full	-> mapM_ (Msg chan >>>) $ playerLine a rivGeoIP
	case poll of
		Left _ -> maybe noluck echofunc (tremulousFindServer polled mess)
		Right aa -> maybe noluck (\x -> echofunc (aa, x)) (M.lookup aa (fst polled))


comTremClans (_, chan, _) = withMasterCache chan $ \(polled,_) -> do
	Config {clanlist}	<- gets config
	case tremulousClanList polled clanlist of
		[]	-> Msg chan >>> "No clans found online."
		str	-> Msg chan >>> intercalate " \STX|\STX " $ take 15 $ map (\(a, b) -> b ++ " " ++ show a) str

comTremStats (_, chan, _) = withMasterCache chan $ \(polled,time) -> do
	now 			<- lift $ getMicroTime
	let (ans, tot, ply, bots) = tremulousStats polled
	Msg chan >>> printf "%d/%d Servers responded with %d players and %d bots. (cache %ds old)"
		ans tot ply bots ((now-time)//1000000)


resolve :: String -> Map String String -> IO (Either IOError SockAddr)
resolve servport localdns = try $ (addrAddress . head) `liftM` getAddrInfo Nothing (Just srv) (Just port)
	where (srv, port) = getIP $ fromMaybe servport (M.lookup (map toLower servport) localdns)


withMasterCache :: String -> ((ServerCache, Integer) -> RiverState) -> RiverState
withMasterCache chan f = do
	rivPoll_		<- gets rivPoll
	(tmD, tmT, tmH)		<- case rivPoll_ of
		Just a	-> return a
		Nothing -> do
			host <- lift $ head `liftM` getAddrInfo Nothing (Just mastersrv) (Just masterport)
			return (undefined, 0, host)
	Config {cacheinterval} 	<- gets config
	now			<- lift $ getMicroTime

	if now-tmT > cacheinterval then do
		newcache <- lift $ try $ tremulousPollAll tmH
		case newcache of
			Left _ | tmT == 0 ->
				Msg chan >>> "Error in fetching Master data."
			Left _ ->
				f (tmD, tmT)
			Right new -> do
				f (new, now)
				modify (\x -> x {rivPoll=Just (new, now, tmH)})
		else f (tmD, tmT)

playerLine :: (SockAddr, ServerInfo) -> GeoIP.Data -> [String]
playerLine (host, (cvars,players_)) geoIP = filter (not . null) echo where
	lookSpc a b = fromMaybe a (lookup b cvars)
	players = sortBy (\(_,a1,_,_) (_,a2,_,_) -> compare a2 a1) $ players_
	avgping = intmean [a | (_,_,a,_) <- players, a /= 999]
	teamfilter filt = intercalate " \STX|\STX " [ircifyColors name++" \SI"++show kills++" ("++show ping++")" | (team, kills, ping, name) <- players, team == filt]
	teamx team filt = case teamfilter filt of
		[]	-> []
		a	-> "\STX"++team++":\STX " ++ a ++ "\n"

	line		= printf "\STXHost:\STX %s \STXName:\STX %s\SI \STXMap:\STX %s \STXPlayers:\STX %s/%s(-%s) \STXØPing:\STX %d \STXCountry:\STX %s"
			(show host) pname pmap pplayers pslots pprivate avgping (pcountry host)
	pname		= stripw . take 50 . ircifyColors . filter isPrint $ lookSpc "[noname]" "sv_hostname"
	pmap		= lookSpc "[nomap]" "mapname"
	pplayers	= show . length $ players
	pslots		= lookSpc "?" "sv_maxclients"
	pprivate	= lookSpc "0" "sv_privateClients"
	pcountry (SockAddrInet _ sip)	= GeoIP.getCountry geoIP (fromIntegral $ flipInt sip)
	pcountry _			= "Unknown" -- This is for IPv6

	echo =	[ line
		, teamx "Aliens" '1'
		, teamx "Humans" '2'
		, teamx "Spectators" '0'
		, teamx "Unknown" '9']

flipInt :: Word32 -> Word32
flipInt old = new where
	new = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
	a = old .&. 0xFF
	b = (old `shiftR` 8 ) .&. 0xFF
	c = (old `shiftR` 16) .&. 0xFF
	d = (old `shiftR` 24) .&. 0xFF
