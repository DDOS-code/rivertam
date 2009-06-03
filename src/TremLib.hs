module TremLib (
	  tremulousFindPlayers
	, tremulousStats
	, tremulousFindServer
	, tremulousClanList
	, tremulousFilter
	, iscomparefunc
	, comparefunc
	, ircifyColors
	, removeColors
) where
import qualified Data.Map as M
import Data.Array hiding ((//))
import Helpers
import TremMasterCache as T
import Network.Socket

tremulousFindPlayers :: ServerCache -> [String] -> [(String, [String])]
tremulousFindPlayers polled input = echo where
	echo 		= filter (not . null . snd) onlyplayers
	onlyplayers 	= [(name (show ip) cvars, [x | PlayerInfo _ _ _ x <- ps, infixFindAny x input']) | (ip, Just (ServerInfo cvars ps)) <- M.toList polled]
	name a b	= maybe a (stripw . take 50 . filter isPrint) (lookup "sv_hostname" b)
	input'		= map (map toLower) input

tremulousFindServer :: ServerCache -> String -> Maybe (SockAddr, ServerInfo)
tremulousFindServer polled searchstring = echo mp where
	echo	[]	= Nothing
	echo	x	= Just $ snd $ minimumBy (\f s -> compare (fst f) (fst s)) x
	mp		= [(a, b) | (Just a, b) <- [(findName (cvars info),(ip, info)) | (ip, Just info) <-M.toList polled]]
	findName x	= case lookup "sv_hostname" x of
		Nothing	-> Nothing
		Just a	-> if isInfixOf (map toLower searchstring) (playerGet a) then Just $ length a else Nothing


tremulousClanList :: ServerCache -> [String] -> [(Int, String)]
tremulousClanList polled clanlist = sortfunc fplayers
	where
	sortfunc	= takeWhile (\(a,_) -> a > 1) . sortBy (\a b -> compare b a)
	fplayers	= map (\a -> (length $ filter (isInfixOf (map toLower a)) players, a)) clanlist
	players		= map (playerGet . piName) . concat . map T.players . catMaybes $ M.elems polled

tremulousStats :: ServerCache -> (Int, Int, Int, Int)
tremulousStats polled = (length servers, tot, players, bots) where
	tot		= M.size polled
	servers		= catMaybes $ M.elems polled
	plist		= concat . map T.players $ servers
	(players, bots) = foldl' trv (0, 0) plist
	trv (!p, !b) PlayerInfo{piPing} = if piPing == 0 then (p, b+1) else (p+1, b)


tremulousFilter :: ServerCache -> String -> (String -> Bool) -> (Int, Int, Int, Int, Int, Int)
tremulousFilter polled fcvar fcmp = foldl' trv (0, 0, 0, 0, 0, 0) playerinfo where
	playerinfo	= catMaybes $ M.elems polled
	trv (!a, !ap, !b, !bp, !c, !cp) (ServerInfo v p)	= case lookup fcvar v of
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
