module Tremulous.Util (
	  findPlayers
	, stats
	, findServer
	, clanList
	, tremulousFilter
	, iscomparefunc
	, comparefunc
	, partitionTeams
	, ircifyColors
	, removeColors
	, webifyColors
) where
import Data.Array hiding ((//))
import Data.List hiding (foldl')
import Data.Maybe
import Data.Function
import Data.Foldable (foldl')
import Helpers
import Tremulous.Protocol as T

findPlayers :: [GameServer] -> [String] -> [(String, [String])]
findPlayers polled input = foldr f [] polled where
	input'	= map (map toLower . stripw) input
	clean	= stripw . take 50 . filter isPrint . removeColors

	f GameServer{address, hostname, players} xs
		| null found	= xs
		| otherwise	= (fromNull (show address) (clean hostname), found) : xs
		where found = filter (`infixFindAny` input') . fmap name $ players


findServer :: [GameServer] -> String -> Maybe GameServer
findServer polled search'' = let
	search	= map toLower search''
	clean	= map toLower . stripw . filter isPrint . removeColors
	findName x 
		| isInfixOf search x	= Just $ length x
		| otherwise		= Nothing

	f srv@GameServer{hostname} = (flip (,) srv) `fmap` findName (clean hostname)

	in case mapMaybe f polled of
		[]	-> Nothing
		xs	-> Just $ snd $ minimumBy (compare `on` fst) xs


clanList :: [GameServer] -> [String] -> [(Int, String)]
clanList polled clanlist = sortfunc fplayers
	where
	sortfunc	= takeWhile (\(a,_) -> a > 1) . sortBy (flip compare)
	fplayers	= map (\a -> (length $ filter (isInfixOf (map toLower a)) players, a)) clanlist
	players		= map (playerGet . name) $ playerList polled

stats :: [GameServer] -> (Int, Int, Int)
stats polled = (tot, players, bots) where
	tot		= length polled
	(players, bots) = foldl' trv (0, 0) (playerList polled)
	trv (!p, !b) x	= if ping x == 0 then (p, b+1) else (p+1, b)


tremulousFilter :: [GameServer] -> String -> String -> String -> (Int, Int, Int, Int, Int, Int)
tremulousFilter polled cvar cmp value = let
	func = maybe (comparefunc cmp value) (intcmp (comparefunc cmp)) (mread value :: Maybe Int)
	in foldl' (trv func) (0, 0, 0, 0, 0, 0) polled
	where
	trv func (!a, !ap, !b, !bp, !c, !cp) GameServer{cvars=v,players=p} = case lookup (Nocase cvar) v of
		Just x	| func x	-> (a+1, ap+pnum, b, bp, c, cp)
			| otherwise	-> (a, ap, b+1, bp+pnum, c, cp)
		Nothing			-> (a, ap, b, bp, c+1, cp+pnum)
		where pnum = length p

	intcmp p val = maybe False (`p` val) . mread

iscomparefunc :: String -> Bool
iscomparefunc x = x `elem` ["==", "/=", ">", ">=", "<", "<="]

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
infixFindAny x = any (`isInfixOf` playerGet x)

partitionTeams :: [PlayerInfo] -> ([PlayerInfo], [PlayerInfo], [PlayerInfo], [PlayerInfo])
partitionTeams = foldr f ([], [], [], []) where
	f x ~(s, a, h, u) = case team x of
		Spectators	-> (x:s, a, h, u)
		Aliens		-> (s, x:a, h, u)
		Humans		-> (s, a, x:h, u)
		Unknown		-> (s, a, h, x:u)

playerList :: [GameServer] -> [PlayerInfo]
playerList = foldr ((++) . T.players) []

playerGet, removeColors, ircifyColors, webifyColors :: String -> String

playerGet = map toLower . removeColors

removeColors = foldr f [] where
	f '^' (x:xs) | x /= '^'	= xs
	f x xs			= x : xs

ircifyColors = foldr f "\SI" where
	f '^' (x:xs) | x >= '0' && x <= '9'	= mc!x ++ xs
	f x xs					= x : xs
	mc = listArray ('0', '9') ["\SI", "\ETX04", "\ETX09", "\ETX08", "\ETX12", "\ETX11", "\ETX13", "\SI", "\SI", "\ETX04"]

webifyColors = f False where
	f n ('^':x:xs) | x >= '0' && x <= '9'
			= close n ++ "<span class=\"t" ++ x : "\">" ++ f True xs
	f n (x:xs)	= x:f n xs
	f n []		= close n
	close n = if n then "</span>" else ""
