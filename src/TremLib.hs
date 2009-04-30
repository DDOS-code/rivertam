module TremLib (
	  tremulousFindSimple
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
import TremMasterCache
import Network.Socket


tremulousFindSimple :: ServerCache -> String -> [(String, [String])]
tremulousFindSimple (polled, _) searchstring = echo where
	echo 		= filter (\(_, b) -> not $ null b) onlyplayers
	onlyplayers 	= [(name (show ip) cvars, [x | (_,_,_,x)<-ps, find' (playerGet x)]) | (ip, (cvars, ps)) <- M.toList polled]
	find'		= isInfixOf (map toLower searchstring)
	name a b	= maybe a (stripw . take 50 . filter isPrint) (lookup "sv_hostname" b)

tremulousFindServer :: ServerCache -> String -> Maybe (SockAddr, ServerInfo)
tremulousFindServer (polled, _) searchstring = echo mp where
	echo	[]	= Nothing
	echo	x	= Just $ snd $ minimumBy (\f s -> compare (fst f) (fst s)) x
	mp		= [(a, b) | (Just a, b) <- [(findName (fst info),(ip, info)) | (ip, info) <-M.toList polled]]
	findName x	= case lookup "sv_hostname" x of
		Nothing	-> Nothing
		Just a	-> if isInfixOf (map toLower searchstring) (playerGet a) then Just $ length a else Nothing


tremulousClanList :: ServerCache -> [String] -> [(Int, String)]
tremulousClanList (polled, _) clanlist = sortfunc fplayers
	where
	sortfunc	= takeWhile (\(a,_) -> a > 1) . sortBy (\a b -> compare b a)
	fplayers	= map (\a -> (length $ filter (isInfixOf (map toLower a)) players, a)) clanlist
	players		= map (\(_,_,_,a) -> playerGet a) . concat $ [p | (_,(_, p)) <-M.toList polled]

tremulousStats :: ServerCache -> (Int, Int, Int, Int)
tremulousStats (polled, unresponsive) = (sresp, stot, players, bots) where
	(sresp, stot)		= (M.size polled, sresp + unresponsive)
	plist			= concat [ps | (_, (_, ps)) <- M.toList polled]
	(players, bots) 	= foldl' trv (0, 0) plist
	trv (!p, !b) (_,_,ping,_) = if ping == 0 then (p, b+1) else (p+1, b)


tremulousFilter :: ServerCache -> String -> (String -> Bool) -> (Int, Int, Int)
tremulousFilter (polled, _) fcvar fcmp = foldl' trv (0, 0, 0) cvars where
	cvars		= [x | (_, (x, _)) <- M.toList polled]
	trv (!a, !b, !c) vars	= case lookup fcvar vars of
		Just x	-> if fcmp x then (a+1, b, c) else (a, b+1, c)
		Nothing	-> (a, b, c+1)

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


playerGet, removeColors, ircifyColors :: String -> String

playerGet = removeColors . map toLower

removeColors []					= []
removeColors (c:[])				= [c]
removeColors (c:cs@(_:css))
	| c == '^'	= removeColors css
	| otherwise	= c : removeColors cs

ircifyColors []					= []
ircifyColors (c:[])				= c:"\SI"
ircifyColors (c:cs@(cs1:css))
	| c == '^' && cs1 >= '0' && cs1 <= '9'	= mc!cs1 ++ ircifyColors css
	| otherwise				= c : ircifyColors cs
	where mc = listArray ('0', '9') ["\SI", "\ETX04", "\ETX09", "\ETX08", "\ETX12", "\ETX11", "\ETX13", "\SI", "\SI", "\ETX04"]
