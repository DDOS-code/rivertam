module Module.Tremulous (mdl) where
import Network.Socket
import Text.Printf
import Control.Exception (try)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Function
import Data.Array hiding ((//))
import Data.String
import qualified Data.ByteString.Char8 as B

import Module hiding (name)
import Module.State
import Module.RiverHDBC
import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Network.Tremulous.Util
import Network.Tremulous.NameInsensitive
import qualified Network.Tremulous.StrictMaybe as S

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
	host		<- io $ mapM mkMaster masterserver
	--geoIP	<- io $ fromFile $ path ++ "IpToCountry.csv"
	modifyCom $ \x -> x
		{ trempoll = HolderTrem 0 host []
		--, geoIP
		}
	where mkMaster (hoststring, proto) = do
		sockAddr <- dnsAddress <$> uncurry getDNS (getIP hoststring)
		return $ MasterServer sockAddr proto

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
	]

comTremFind, comTremStats, comTremClans :: Command State
comTremServer :: Mode -> Command State

comTremFind = withMasterCache $ \polled _ mess' -> let
	fixline (srv, players) = view srv ('\SI' : intercalate "\SI \STX|\STX " players)
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
			EchoM >>> map fixline a

comTremServer mode mess_ = do
	dnsfind	<- resolve mess_
	case dnsfind of
		Left _ -> withMasterCache  (\polled _ mess -> case findServer polled mess of
			Nothing	-> Echo >>> view mess "Not found."
			Just a	-> echofunc a) mess_
		Right host -> do
			response <- io $ pollOne defaultDelay (dnsAddress host)
			case response of
				S.Nothing -> Echo >>> view mess_ "No response."
				S.Just a  -> echofunc a
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
			Just a	-> filter (\y -> protocol y == a) p
			Nothing	-> p

	if now-pollTime <= interval
		then
			f (ff poll) pollTime strnew
		else do
			new <- io $ polled <$> pollMasters defaultDelay masters
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
	"\STX[\STX" ++ (protoToAbbr protocol) ++ "\STX]\STX " ++
	(unwords $ fmap (uncurry view)
	([ ("Host"	, show address)
	, ("Name"	, sanitize (B.unpack . original $ hostname))
	, ("Map"	, S.maybe "?" prettystring mapname)
	, ("Players"	, (show nplayers) ++ "/" ++ show slots ++ "(+"++ show privslots++")")
	] ++ if nplayers > 0
		then [("ØPing"	, show $ intmean . filter validping . map ping $ players)]
		else [])
	)
	where
	sanitize	= ircifyColors . stripw . take 50 . filter (\x -> let a = ord x in a >= 32 && a <= 127)
	validping x	= x > 0 && x < 999


serverPlayers :: [Player] -> [String]
serverPlayers players'' = let
	players		= sortBy (flip compare `on` kills) $ players''
	(specs, aliens, humans, unknown) = partitionTeams players
	in fmap teamFormat $ filter (not . null) [aliens, humans, specs, unknown]
	where
	teamFormat xs	= view (show (team $ head xs)) $ playersFormat xs
	playersFormat 	= intercalate " \STX|\STX " . map (\(Player _ kills ping name) -> prettystring name ++ "\SI " ++ show kills ++ " " ++ show ping)


clanList :: [GameServer] -> [String] -> [(Int, String)]
clanList polled clanlist = sortfunc fplayers
	where
	sortfunc	= takeWhile (\(a,_) -> a > 1) . sortBy (flip compare)
	fplayers	= map (\a -> (count (isInfixOf (map toLower a)) plys, a)) clanlist
	plys		= map (cleanstring . name) $ concatMap players polled
	count p		= length . filter p



findPlayers :: [GameServer] -> [String] -> [(String, [String])]
findPlayers polled input = foldr f [] polled where
	input'	= map (map toLower . stripw) input
	f GameServer{address, hostname, players} xs
		| null found	= xs
		| otherwise	= (fromNull (show address) (prettystring hostname), found) : xs
		where found = map prettystring $
			filter (\x -> any (`isInfixOf` cleanstring x) input) . fmap name $ players


findServer :: [GameServer] -> String -> Maybe GameServer
findServer polled search'' = let
	search	= map toLower search''
	findName x
		| isInfixOf search x	= Just $ length x
		| otherwise		= Nothing

	f srv@GameServer{hostname} = (flip (,) srv) `fmap` findName (cleanstring hostname)

	in case mapMaybe f polled of
		[]	-> Nothing
		xs	-> Just $ snd $ minimumBy (compare `on` fst) xs



prettystring, cleanstring :: TI -> String
prettystring = ircifyColors . B.unpack . original
cleanstring = B.unpack . cleanedCase

infixFindAny :: String -> [TI] -> Bool
infixFindAny x = any (`isInfixOf` x) . map cleanstring

ircifyColors :: String -> String
ircifyColors = foldr f "\SI" where
	f '^' (x:xs) | isDigit x = mc!x ++ xs
	f x xs					= x : xs
	mc = listArray ('0', '9') ["\SI", "\ETX04", "\ETX09", "\ETX08", "\ETX12", "\ETX11", "\ETX13", "\SI", "\SI", "\ETX04"]


protoToAbbr, protoToFull :: Int ->  String
protoToAbbr x = case x of
	69 -> "1.1"
	70 -> "gpp"
	86 -> "unv"
	_  -> "?"

protoToFull x = case x of
	69 -> "Tremulous 1.1"
	70 -> "Tremulous GPP"
	86 -> "Unvanquished"
	_  -> "<Unknown>"

string2proto :: String -> Maybe Int
string2proto x = case x of
	"vanilla"	-> Just 69
	"1.1"		-> Just 69
	"gpp"		-> Just 70
	"1.2"		-> Just 70
	_		-> Nothing