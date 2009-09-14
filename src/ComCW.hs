module ComCW (list, initialize) where
import Control.Monad hiding (mapM_)
import Text.Printf
import System.Time
import Database.HDBC
import Data.Foldable
import Prelude hiding (id, map, any, mapM_, all, elem, sum)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM

import CommandInterface

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
	, ("cw-detailed"	, (cwDetailed	, 0	, Peon	, "(clan)"
		, "Detailed stats about the clangames. (Use the argument for clan-filtering)"))
	, ("cw-lastgame"	, (cwLast	, 0	, Peon	, ""
		, "Last played clangame."))
	, ("cw-addgame"		, (cwAddGame	, 1	, User	, "<clan> (unix-timestamp)"
		, "Adds a game. Supply an additional timestamp in the unix format, or have it default to now."))
	, ("cw-rmgame"		, (cwRmGame	, 1	, User	, "<id>"
		, "Removes a game including the associated rounds and comments. Be careful since this can't be undone."))
	, ("cw-addround"	, (cwAddRound	, 3	, User	, "<id> <map> <score>"
		, "Assigns a round to a game. Example: 'cw-addround 3 niveus ww'. Score legend: \STXw\STXon, \STXl\STXost, \STXd\STXraw, \STXn\STXot played."))
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

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	let maybeCreate name value = unless (name `elem` tables) (run conn value [] >> return ())
	maybeCreate "cw_games" cw_games
	maybeCreate "cw_rounds" cw_rounds
	commit conn
	where
	cw_games = "CREATE TABLE cw_games (\
		\    id      SERIAL PRIMARY KEY,\
		\    clan    TEXT NOT NULL,\
		\    unix    INTEGER NOT NULL\
		\)"
	cw_rounds = "CREATE TABLE cw_rounds (\
		\    id      SERIAL PRIMARY KEY,\
		\    cw_game INTEGER REFERENCES cw_games ON UPDATE CASCADE ON DELETE CASCADE,\
		\    map     TEXT NOT NULL,\
		\    ascore  CHAR(1) NOT NULL,\
		\    hscore  CHAR(1) NOT NULL\
		\)"

cwAddGame, cwRmGame, cwAddRound, cwListGames, cwGame, cwDetailed, cwLast, cwOpponents, cwSummary :: Command

cwAddGame _ mess Info{echo} ComState{conn} = let
	err _	= rollback conn >> echo "Adding game Failed."
	try	= do
		TOD now _ <- getClockTime
		let unix = fromMaybe now (mread $ firstWord timestamp)
		run conn "INSERT INTO cw_games (clan, unix) VALUES (?, ?)"
			[toSql opponent, toSql unix]
		commit conn
		[[id]] <- quickQuery' conn "SELECT id FROM cw_games ORDER BY id DESC LIMIT 1" []
		echo $ "Game versus " ++ opponent ++ " added with id " ++ fromSql id ++ "."
	in handleSql err try
	where (opponent, timestamp) = breakDrop isSpace mess

cwRmGame _ mess Info{echo} ComState{conn} = let
	err _	= rollback conn >> echo "Removing game failed. Should not happen."
	try	= do
		query	<- quickQuery' conn "SELECT clan FROM cw_games WHERE id = ?" [toSql id]
		case query of
			[[clan]] -> do
				run conn "DELETE FROM cw_games WHERE id = ?" [toSql id]
				commit conn
				echo $ "Game ("++id++")" ++ fromSql clan ++ " successfully removed."
			_	-> echo $ "Game id ("++id++") not found."
	in handleSql err try
	where id = firstWord mess

cwAddRound _ mess Info{echo} ComState{conn} = case words mess of
	[id, map',[as,hs]] | okscore as && okscore hs -> let
		err _	= rollback conn >> echo "Adding round Failed. Perhaps the id is incorrect?"
		try	= do
			run conn "INSERT INTO cw_rounds (cw_game, map, ascore, hscore) VALUES (?, ?, ?, ?)"
				[toSql id, toSql map', toSql as, toSql hs]
			commit conn
			[[clan]] <- quickQuery' conn "SELECT clan FROM cw_games WHERE id = ?" [toSql id]
			echo $ "Round added to ("++id++")"++fromSql clan++"."
		in handleSql err try
	_	-> echo "\STXcw-addround:\STX Error in syntax."
	where okscore x = x `elem` "wldn"

cwListGames _ mess Info{echo} ComState{conn} = do
	[[num]]	<- quickQuery' conn "SELECT COUNT(*) FROM cw_games" []
	let	num'	= fromSql num :: Int
		start	= inrange 0 num' $ (fromMaybe (num'-10) $ mread $ firstWord mess :: Int)
		end	= min num' (start + 10)
 	q	<- quickQuery conn "SELECT id,clan FROM cw_games ORDER BY unix, id LIMIT 10 OFFSET ?" [toSql start]
	echo $ printf "Games %d-%d of %d: %s" start end num' (intercalate ", " (fmap format q))

	where	format = \[id, clan] -> "(" ++ fromSql id ++ ")" ++ fromSql clan
		inrange x y = max x . min y

cwGame _ mess Info{echo} ComState{conn} = do
	q <- quickQuery' conn "SELECT * FROM cw_games WHERE id = ?" [toSql $ fromMaybe (0::Int) (mread mess)]
	case q of
		[[id, clan, unix]] -> do
			maps	<- quickQuery' conn "SELECT map FROM cw_rounds WHERE cw_game = ?" [id]
			scores	<- quickQuery' conn "SELECT ascore,hscore FROM cw_rounds WHERE cw_game = ?" [id]
			TOD now _ 	<- getClockTime
			let	id'	= fromSql id :: Int
				maps'	= intercalate ", " $ fmap (\[a] -> fromSql a) maps
				clan'	= fromSql clan :: String
				time	= (now - (fromSql unix)) // (60*60*24)
				sqlToScore xs = let f = toScore . fromSql in [a+h | [f -> Just a, f -> Just h] <- xs]
				Score tW tL tD	= sum $ sqlToScore scores

			echo $ printf "\STX(\STX%d\STX)%s:\STX %d days ago on %s \STXRounds:\STX %d won, %d lost and %d draw."
				id' clan' time maps' tW tL tD

		_ -> echo $ "Id \"" ++ mess ++ "\": Not found."

summary :: [(Int, Score)] -> String
summary lst = let
	m		= IM.fromListWith (+) lst
	Score gW gL gD	= foldl' gamecount 0 m
	Score tW tL tD	= sum m
	gTot		= IM.size m
	tTot		= tW + tL + tD
	in printf "\STX%d Games: %.1f%% won\STX (won: %d, lost: %d, draw: %d) || \STX%d Rounds: %.1f%% won\STX (won: %d, lost: %d, draw: %d)"
			gTot (ratio gW gTot) gW gL gD  tTot (ratio tW tTot) tW tL tD
	where
	gamecount (Score a b c) (Score w l _)
		| w > l		= Score (a+1) b c
		| w < l		= Score a (b+1) c
		| otherwise	= Score a b (c+1)

	ratio x tot = 100 * (fromIntegral x) / (fromIntegral tot) :: Double

cwSummary _ mess Info{echo} ComState{conn} = do
	query	<- uncurry (quickQuery conn) fetch
	echo $ (if null arg then "" else "\STX"++arg++":\STX ") ++ case format query of
		[]	-> "No games played."
		x	->  summary x
	where	arg	= firstWord mess
		fetch	= if null arg
			then ("SELECT cw_game,ascore,hscore FROM cw_rounds", [])
			else ("SELECT cw_game,ascore,hscore FROM cw_rounds JOIN cw_games ON cw_game = cw_games.id WHERE cw_games.clan ILIKE ?", [toSql arg])
		format xs = let f = toScore . fromSql in [(fromSql id, a + h) | [id, f -> Just a, f -> Just h] <- xs]

cwOpponents _ _ Info{echo} ComState{conn} = do
	query	<- quickQuery conn "SELECT clan FROM (SELECT DISTINCT ON (UPPER(clan)) * FROM cw_games) clans ORDER BY unix" []
	echo $ intercalate ", " $ fmap (\[a] -> fromSql a) $ query

cwLast nick _ info c@ComState{conn} = do
	q <- quickQuery' conn "SELECT id FROM cw_games ORDER BY unix DESC LIMIT 1" []
	case q of
		[[a]]	-> cwGame nick (fromSql a) info c
		_	-> echo info $ "No games played."

detailed :: [(Nocase, (Score, Score))] -> [String]
detailed cgs = let
	merged					= M.fromListWith addup cgs
	(Score taW taL taD, Score thW thL thD)	= foldl' addup (0, 0) $ M.elems merged
	format = fmap f (M.toList merged)
	in	"\STXMap\ETX4            aW  aL  aD\ETX12      hW  hL  hD":
		format ++
		[printf "\STXTotal\ETX4        %4d%4d%4d\ETX12    %4d%4d%4d" taW taL taD thW thL thD]
	where	f (Nocase map', (Score aW aL aD, Score hW hL hD)) =
			printf "%-13s\ETX4%4d%4d%4d\ETX12    %4d%4d%4d" map' aW aL aD hW hL hD
		addup (!a1, !h1) (!a2, !h2) = (a1+a2, h1+h2)

cwDetailed _ mess Info{echo} ComState{conn} = do
	query	<- uncurry (quickQuery conn) fetch
	case formatSql query of
		[]	-> echo $ "\STX"++arg++"\STX: No games played."
		x	-> mapM_ echo $ detailed x
	where	arg = firstWord mess
		formatSql xs = let f = toScore . fromSql in [(Nocase $ fromSql id, (a, h)) | [id, f -> Just a, f -> Just h] <- xs]
		fetch = if null arg
			then ("SELECT map,ascore,hscore FROM cw_rounds", [])
			else ("SELECT map,ascore,hscore FROM cw_rounds JOIN cw_games ON cw_game = cw_games.id WHERE cw_games.clan ILIKE ?", [toSql arg])
