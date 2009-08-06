module ComCW (list, initialize) where
import Control.Monad hiding (mapM_)
import Text.Printf
import System.Time
import Database.HDBC
import Data.List (intercalate)
import Data.Foldable
import Prelude hiding (id, map, any, mapM_, all, elem)
import Data.Maybe
import qualified Data.Map as M

import CommandInterface
import Config
import Helpers

list :: CommandList
list =
	[ ("cw-summary"		, (cwSummary	, 0	, Peon	, "(clan)"
		, "A summary of rounds played. (Use the argument for clan-filtering)"))
	, ("cw-listgames"	, (cwListGames	, 0	, Peon	, "(start-id)"
		, "List of the 10 latest games with start-id as the beginning."))
	, ("cw-game"		, (cwGame	, 1	, Peon	, "<id>"
		, "Info about a particular game."))
	, ("cw-opponents"	, (cwOpponents	, 0	, Peon	, ""
		, "List every oppenent the clan has played against."))
	, ("cw-detailed"	, (cwDetailed	, 0	, Peon	, ""
		, "Detailed stats about the clangames. (Use the argument for clan-filtering)"))
	, ("cw-lastgame"	, (cwLast	, 0	, Peon	, ""
		, "Last clangame that was played."))
	, ("cw-addgame"		, (cwAddGame	, 1	, User	, "<clan>"
		, ""))
	, ("cw-addround"	, (cwAddRound	, 3	, User	, "<id> <map> <score>"
		, ""))
	, ("cw-addcomment"	, (cwAddComment	, 2	, Peon	, "<id> <<comment>>"
		, ""))
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

sqlToScore :: [[SqlValue]] -> [(Score, Score)]
sqlToScore xs = let f = toScore . fromSql in [(a, h) | [f -> Just a, f -> Just h] <- xs]

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	let maybeCreate name value = unless (name `elem` tables) (run conn value [] >> return ())
	maybeCreate "cw_games" cw_games
	maybeCreate "cw_rounds" cw_rounds
	maybeCreate "cw_comments" cw_comments
	commit conn
	where
	cw_games = "CREATE TABLE cw_games (\
		\    id      INTEGER PRIMARY KEY AUTOINCREMENTz,\
		\    clan    TEXT NOT NULL COLLATE NOCASE,\
		\    unix    INTEGER NOT NULL\
		\)"
	cw_rounds = "CREATE TABLE cw_rounds (\
		\    id      INTEGER PRIMARY KEY AUTOINCREMENT,\
		\    cw_game INTEGER NOT NULL,\
		\    map     TEXT NOT NULL COLLATE NOCASE,\
		\    asc     TEXT NOT NULL,\
		\    hsc     TEXT NOT NULL\
		\)"
	cw_comments = "CREATE TABLE cw_comments (\
		\    id      INTEGER PRIMARY KEY AUTOINCREMENT,\
		\    cw_game INTEGER NOT NULL,\
		\    nick    TEXT NOT NULL COLLATE NOCASE,\
		\    value   TEXT NOT NULL\
		\)"

cwAddGame, cwAddComment, cwAddRound, cwListGames, cwGame, cwDetailed, cwLast, cwOpponents, cwSummary :: Command

cwAddGame _ mess Info{echo} ComState{conn} = handleSql
	(const $ echo "Adding game Failed.") $ do
		TOD unix _ 	<- getClockTime
		run conn "INSERT INTO cw_games VALUES (NULL, ?, ?)" [toSql opponent, toSql unix]
		commit conn
		[[id]] <- quickQuery' conn "SELECT last_insert_rowid()" []
		echo $ "Game versus " ++ opponent ++ " added with id " ++ fromSql id ++ "."
	where opponent = firstWord mess

cwAddComment nick mess Info{echo} ComState{conn} = handleSql (const $ echo "Adding comment Failed. Perhaps the id is incorrect?") $ do
	run conn "INSERT INTO cw_comments VALUES (NULL, ?, ?, ?)" [toSql id, toSql nick, toSql comment]
	commit conn
	echo "Comment added."
	where (id, comment) = breakDrop isSpace mess

cwAddRound _ mess Info{echo} ComState{conn} = case words mess of
	[id, map',[as,hs]] | okscore as && okscore hs ->
		handleSql (const $ echo "Adding round Failed. Perhaps the id is incorrect?") $ do
			run conn "INSERT INTO cw_rounds VALUES (NULL, ?, ?, ?, ?)"
				[toSql id, toSql map', toSql as, toSql hs]
			commit conn
			echo "Round added."
	_	-> echo "\STXcw-addround:\STX Error in syntax."
	where okscore x = x `elem` "wldn"

cwListGames _ mess Info{echo} ComState{conn} = do
	[[num]]	<- quickQuery' conn "SELECT COUNT(*) FROM cw_games" []
	let	num'	= fromSql num :: Int
		start	= inrange 0 num' $ (fromMaybe (num'-10) $ mread $ firstWord mess :: Int)
		end	= min num' (start + 10)
 	q	<- quickQuery conn "SELECT id,clan FROM cw_games ORDER BY unix LIMIT ?, ?" [toSql start, toSql end]
	echo $ printf "Games %d-%d of %d: %s" start end num' (intercalate ", " (fmap format q))

	where	format = \[id, clan] -> "(" ++ fromSql id ++ ")" ++ fromSql clan
		inrange x y = max x . min y

cwGame _ mess Info{echo} ComState{conn} = do
	q <- quickQuery' conn "SELECT * FROM cw_games WHERE id = ?" [toSql mess]
	case q of
		[[id, clan, unix]] -> do
			maps	<- quickQuery' conn "SELECT map FROM cw_rounds WHERE cw_game = ?" [id]
			scores	<- quickQuery' conn "SELECT asc,hsc FROM cw_rounds WHERE cw_game = ?" [id]
			TOD now _ 	<- getClockTime
			let	id'	= fromSql id :: Int
				maps'	= intercalate ", " $ fmap (\[a] -> fromSql a) maps
				clan'	= fromSql clan :: String
				time	= (now - (fromSql unix)) // (60*60*24)
				Score tW tL tD	= foldl' (\acc (a, h) -> acc + a + h) 0 $ sqlToScore scores

			echo $ printf "%d - \STX%s\STX - %d days ago - %s - \STXRounds:\STX %d won, %d lost and %d draw."
				id' clan' time maps' tW tL tD

		_ -> echo $ "Id \"" ++ mess ++ "\": Not found."

summary :: [(Score, Score)] -> String
summary lst = let
	Score tW tL tD	= foldl' (\acc (a, h) -> acc + a + h) 0 lst
	tot		= tW + tL + tD
	winratio	= 100.0 * (fromIntegral tW) / (fromIntegral tot) :: Double
	in printf "rounds: %d | won: %d / lost: %d / draw: %d | %.1f%% won"
			tot tW tL tD winratio

cwSummary _ mess Info{echo} ComState{conn} = do
	query	<- uncurry (quickQuery conn) fetch
	echo $ (if null arg then "" else"\STX"++arg++"\STX: ") ++ case query of
		[]	-> "No games played."
		x	->  summary $ sqlToScore x
	where	arg	= firstWord mess
		fetch	= if null arg
			then ("SELECT asc,hsc FROM cw_rounds", [])
			else ("SELECT asc,hsc FROM cw_rounds JOIN cw_games ON cw_game = cw_games.id WHERE cw_games.clan = ?", [toSql arg])

cwOpponents _ _ Info{echo} ComState{conn} = do
	query	<- quickQuery conn "SELECT DISTINCT clan FROM cw_games" []
	echo $ intercalate ", " $ fmap (\[a] -> fromSql a) $ query

cwLast nick _ info c@ComState{conn} = do
	[[q]] <- quickQuery' conn "SELECT id FROM cw_games ORDER BY unix DESC LIMIT 1" []
	cwGame nick (fromSql q) info c

detailed :: [(String, Score, Score)] -> [String]
detailed cgs = let
	merged					= mergemaps cgs
	(Score taW taL taD, Score thW thL thD)	= foldl' addup (0, 0) $ M.elems merged
	format = fmap f (M.toList merged)
	in	"\STXMap\ETX4           aW  aL  aD\ETX12      hW  hL  hD":
		format ++
		[printf "\STXTotal\ETX4         %2d  %2d  %2d\ETX12      %2d  %2d  %2d" taW taL taD thW thL thD]
	where	f (map', (Score aW aL aD, Score hW hL hD)) =
			printf "%-13s\ETX4 %2d  %2d  %2d\ETX12      %2d  %2d  %2d" map' aW aL aD hW hL hD

		mergemaps = foldl' (\m (map, asc, hsc) -> M.insertWith addup map (asc, hsc) m) M.empty
		addup (!a1, !h1) (!a2, !h2) = (a1+a2, h1+h2)

cwDetailed _ mess Info{echo} ComState{conn} = do
	query	<- uncurry (quickQuery conn) fetch
	case fmap formatSql query of
		[]	-> echo $ "\STX"++arg++"\STX: No games played."
		x	-> mapM_ echo $ detailed x
	where	arg = firstWord mess
		formatSql = \[a, b, c] -> (fromSql a, fromJust $ toScore $ fromSql b, fromJust $ toScore $ fromSql c)
		fetch = if null arg
			then ("SELECT map,asc,hsc FROM cw_rounds", [])
			else ("SELECT map,asc,hsc FROM cw_rounds JOIN cw_games ON cw_game = cw_games.id WHERE cw_games.clan = ?", [toSql arg])
