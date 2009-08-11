module TremLib (
	  tremulousFindPlayers
	, tremulousStats
	, tremulousFindServer
	, tremulousClanList
	, tremulousFilter
	, iscomparefunc
	, comparefunc
	, partitionTeams
	, ircifyColors
	, removeColors
) where
import qualified Data.Map as M
import Data.Array hiding ((//))
import Data.List
import Data.Maybe
import Helpers
import TremPolling as T
import Network.Socket

tremulousFindPlayers :: PollResponse -> [String] -> [(String, [String])]
tremulousFindPlayers polled input = echo where
	echo 		= filter (not . null . snd) onlyplayers
	onlyplayers 	= [(name (show ip) cvars, [x | PlayerInfo _ _ _ x <- ps, infixFindAny x input']) | (ip, ServerInfo cvars ps) <- M.toList polled]
	name a b	= maybe a (stripw . take 50 . filter isPrint) (lookup (Nocase "sv_hostname") b)
	input'		= map (map toLower) input

tremulousFindServer :: PollResponse -> String -> Maybe (SockAddr, ServerInfo)
tremulousFindServer polled searchstring = echo mp where
	echo	[]	= Nothing
	echo	x	= Just $ snd $ minimumBy (\f s -> compare (fst f) (fst s)) x
	mp		= [(a, b) | (Just a, b) <- [(findName (cvars info),(ip, info)) | (ip, info) <-M.toList polled]]
	findName x	= case lookup (Nocase "sv_hostname") x of
		Nothing	-> Nothing
		Just a	-> if isInfixOf (map toLower searchstring) (playerGet a) then Just $ length a else Nothing


tremulousClanList :: PollResponse -> [String] -> [(Int, String)]
tremulousClanList polled clanlist = sortfunc fplayers
	where
	sortfunc	= takeWhile (\(a,_) -> a > 1) . sortBy (\a b -> compare b a)
	fplayers	= map (\a -> (length $ filter (isInfixOf (map toLower a)) players, a)) clanlist
	players		= map (playerGet . piName) . concat . map T.players $ M.elems polled

tremulousStats :: PollResponse -> (Int, Int, Int)
tremulousStats polled = (tot, players, bots) where
	tot		= M.size polled
	plist		= concat . map T.players . M.elems $ polled
	(players, bots) = foldl' trv (0, 0) plist
	trv (!p, !b) x = if piPing x == 0 then (p, b+1) else (p+1, b)


tremulousFilter :: PollResponse -> String -> (String -> Bool) -> (Int, Int, Int, Int, Int, Int)
tremulousFilter polled fcvar fcmp = foldl' trv (0, 0, 0, 0, 0, 0) playerinfo where
	playerinfo	= M.elems polled
	trv (!a, !ap, !b, !bp, !c, !cp) (ServerInfo v p)	= case lookup (Nocase fcvar) v of
		Just x	-> if fcmp x then (a+1, ap+pnum, b, bp, c, cp) else (a, ap, b+1, bp+pnum, c, cp)
		Nothing	-> (a, ap, b, bp, c+1, cp+pnum)
		where pnum = length p

iscomparefunc :: String -> Bool
iscomparefunc x = any (==x) ["==", "/=", ">", ">=", "<", "<="]

comparefunc :: (Ord a) => String -> (a -> a -> Bool)
comparefunc x = case x of
	"=="	-> (==)
	"/="	-> (/=)
	">"	-> (>)
	">="	-> (>=)
	"<"	-> (<)
	"<="	-> (<=)
	_	-> error "comparefunc: Not found. (This shouldn't happen)"

infixFindAny :: String -> [String] -> Bool
infixFindAny x = any (\a -> isInfixOf a (playerGet x))

partitionTeams :: [PlayerInfo] -> ([PlayerInfo], [PlayerInfo], [PlayerInfo], [PlayerInfo])
partitionTeams = foldr f ([], [], [], []) where
	f x ~(s, a, h, u) = case piTeam x of
		Spectators	-> (x:s, a, h, u)
		Aliens		-> (s, x:a, h, u)
		Humans		-> (s, a, x:h, u)
		Unknown		-> (s, a, h, x:u)

playerGet, removeColors, ircifyColors :: String -> String

playerGet = removeColors . map toLower

removeColors []				= []
removeColors ('^':x:xs) | x /= '^'	= removeColors xs
removeColors (x:xs)			= x : removeColors xs

ircifyColors []					= []
ircifyColors (c:[])				= c:"\SI"
ircifyColors (c:cs@(cs1:css))
	| c == '^' && cs1 >= '0' && cs1 <= '9'	= mc!cs1 ++ ircifyColors css
	| otherwise				= c : ircifyColors cs
	where mc = listArray ('0', '9') ["\SI", "\ETX04", "\ETX09", "\ETX08", "\ETX12", "\ETX11", "\ETX13", "\SI", "\SI", "\ETX04"]
