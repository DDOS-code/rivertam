module ComCW (list, initialize) where
import Control.Monad hiding (mapM_)
import Text.Printf
import System.Time
import System.Locale
import Database.HDBC
import Data.List (intercalate)
import Data.Foldable
import Prelude hiding (id, map, any, mapM_)
import Data.Maybe
import qualified Data.Map as M

import CommandInterface
import Config
import Helpers

list :: CommandList
list =
	[ ("cw-summary"		, (cwSummary	, 0	, Peon	, "(clan)"
		, "A summary of rounds played. (Use the argument for clan-filtering)"))
	, ("cw-opponents"	, (cwOpponents	, 0	, Peon	, ""
		, "List every oppenent the clan has played against."))
	, ("cw-detailed"	, (cwDetailed	, 0	, Peon	, ""
		, "Detailed stats about the clangames. (Use the argument for clan-filtering)"))
	, ("cw-lastgame"	, (cwLast	, 0	, Peon	, ""
		, "Last clangame that was played."))
	, ("cw-add"		, (cwAdd	, 3	, User	, "<clanname> <map> <score>"
		, "Add a clangame to the database. Example: \"ddos niveus wd\". For the last field: (w)on/(l)ost/(d)raw/(n)ot played, first aliens then humans."))
	]

data Score = Score !Int !Int !Int deriving (Eq, Show)

instance (Num Score) where
	(Score a1 b1 c1) + (Score a2 b2 c2) = Score (a1+a2) (b1+b2) (c1+c2)
	abs		= undefined
	(*)		= undefined
	signum		= undefined
	fromInteger x	= let x' = fromInteger x in Score x' x' x'

toScore :: Char -> Maybe Score
toScore c = case c of
	'w'	-> Just $ Score 1 0 0
	'l'	-> Just $ Score 0 1 0
	'd'	-> Just $ Score 0 0 1
	'n'	-> Just $ Score 0 0 0
	_	-> Nothing

data ClanGame = CG {
	id
	, unix	:: Integer
	, clan
	, map	:: String
	, asc
	, hsc	:: Score
	}

sqlToCG :: [SqlValue] -> ClanGame
sqlToCG  = \[id', unix', clan', map', asc', hsc'] -> CG {
	id	= fromSql id'
	, unix	= fromSql unix'
	, clan	= fromSql clan'
	, map	= fromSql map'
	, asc	= fromJust $ toScore $ fromSql asc'
	, hsc	= fromJust $ toScore $ fromSql hsc'
	}

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless (any (=="cw") tables) $ do
		run conn create []
		commit conn
	where
	create = "CREATE TABLE cw ("
		++ "id       INTEGER PRIMARY KEY AUTOINCREMENT,"
		++ "unix     INTEGER,"
		++ "clan     TEXT NOT NULL COLLATE NOCASE,"
		++ "map      TEXT NOT NULL COLLATE NOCASE,"
		++ "asc      TEXT NOT NULL,"
		++ "hsc      TEXT NOT NULL"
		++ ")"


cwAdd, cwSummary, cwOpponents, cwLast, cwDetailed :: Command
cwAdd _ mess Info{echo} ComState{conn} = case words mess of
	[clan,map',[as,hs]] | okscore as && okscore hs -> do
		TOD unix _ 	<- getClockTime
		run conn save [toSql unix, toSql clan, toSql map', toSql as, toSql hs]
		commit conn
		echo $ "Clangame added."
	_	-> echo "\STXcw-add:\STX Error in syntax."
	where	save	= "INSERT INTO cw VALUES (NULL, ?, ?, ?, ?, ?)"
		okscore	x = any (x==) "wldn"

summary :: [(Score, Score)] -> String
summary lst = let
	Score tW tL tD	= foldl' (\acc (a, h) -> acc + a + h) 0 lst
	tot		= tW + tL + tD
	winratio	= 100.0 * (fromIntegral tW) / (fromIntegral tot) :: Double
	in printf "rounds: %d | won: %d / lost: %d / draw: %d | %.1f%% won"
			tot tW tL tD winratio

cwSummary _ mess Info{echo} ComState{conn}
	| null mess	= do
		query	<- sqlToScore `liftM` quickQuery conn "SELECT asc,hsc FROM cw" []
		echo $ summary $ query
	| otherwise	= do
		query	<- quickQuery conn "SELECT asc,hsc FROM cw WHERE clan = ?" [toSql arg]
		echo $ "\STX"++arg++"\STX: " ++ case query of
			[]	-> "No games played."
			x	->  summary . sqlToScore $ x
	where	arg	= firstWord mess
		sqlToScore xs = let f = toScore . fromSql in [(a, h) | [f -> Just a, f -> Just h] <- xs]


cwOpponents _ _ Info{echo} ComState{conn} = do
	query	<- quickQuery conn "SELECT DISTINCT clan FROM cw" []
	echo $ intercalate ", " $ fmap (\[a] -> fromSql a) $ query

cwLast _ _ Info{echo} ComState{conn} = do
	query	<- quickQuery' conn "SELECT * FROM cw ORDER BY unix DESC LIMIT 1" []
	echo $ case query of
		[x] -> let cg = sqlToCG x in
			printf "Last clangame was a %s versus \STX%s\STX on %s, %s."
				(roundwld (asc cg + hsc cg)) (clan cg) (map cg) (timestr $ unix cg)
		_ -> "No clangames played."
	where
	timestr	unix = formatCalendarTime defaultTimeLocale "%c" (toUTCTime (TOD unix 0))

roundwld :: Score -> String
roundwld (Score w l _)
	| w > l		= "victory"
	| w < l		= "defeat"
	| otherwise	= "draw"

detailed :: [ClanGame] -> [String]
detailed cgs = let
	merged					= mergemaps cgs
	(Score taW taL taD, Score thW thL thD)	= foldl' addup (0, 0) $ M.elems merged
	format = fmap f (M.toList merged)
	in	"\STXMap\ETX4           aW  aL  aD\ETX12      hW  hL  hD":
		format ++
		[printf "\STXTotal\ETX4         %2d  %2d  %2d\ETX12      %2d  %2d  %2d" taW taL taD thW thL thD]
	where	f (map', (Score aW aL aD, Score hW hL hD)) =
			printf "%-13s\ETX4 %2d  %2d  %2d\ETX12      %2d  %2d  %2d" map' aW aL aD hW hL hD

		mergemaps = foldl' (\m CG{map, asc, hsc} -> M.insertWith addup map (asc, hsc) m) M.empty
		addup (!a1, !h1) (!a2, !h2) = (a1+a2, h1+h2)

cwDetailed _ mess Info{echo} ComState{conn}
	| null mess	= do
		query	<- fmap sqlToCG `liftM` quickQuery conn "SELECT * FROM cw" []
		mapM_ echo $ detailed query
	| otherwise	= do
		query	<- fmap sqlToCG `liftM` quickQuery conn "SELECT * FROM cw WHERE clan = ?" [toSql arg]
		case query of
			[]	-> echo $ "\STX"++arg++"\STX: No games played."
			x	->  mapM_ echo $ detailed x
	where arg = firstWord mess
