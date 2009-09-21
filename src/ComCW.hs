module ComCW (m) where
import CommandInterface
import Data.List (intercalate)
import Text.Printf
import Database.HDBC
import Data.Foldable
import Prelude hiding (id, map, any, all, elem, sum)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM

m :: Module
m = Module
	{ modName	= "clanwar"
	, modInit	= sqlIfNotTable "cw_games" [cw_games] >> sqlIfNotTable "cw_rounds" [cw_rounds]
	, modFinish	= return ()
	, modList	= list
	}

cw_games, cw_rounds :: String
cw_games = "CREATE TABLE cw_games (\
	\    id      SERIAL PRIMARY KEY,\
	\    clan    INTEGER REFERENCES clans ON DELETE RESTRICT,\
	\    unix    INTEGER NOT NULL\
	\)"
cw_rounds = "CREATE TABLE cw_rounds (\
	\    id      SERIAL PRIMARY KEY,\
	\    cw_game INTEGER REFERENCES cw_games ON UPDATE CASCADE ON DELETE CASCADE,\
	\    map     TEXT NOT NULL,\
	\    ascore  CHAR(1) NOT NULL,\
	\    hscore  CHAR(1) NOT NULL\
	\)"

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
	, ("cw-lastgame"	, (cwLastGame	, 0	, Peon	, ""
		, "Last played clangame."))
	, ("cw-addgame"		, (cwAddGame	, 1	, User	, "<clan> (unix-timestamp)"
		, "Adds a game. Supply an additional timestamp in the unix format, or have it default to now."))
	, ("cw-delgame"		, (cwDelGame	, 1	, User	, "<id>"
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



cwAddGame, cwDelGame, cwAddRound, cwListGames, cwGame, cwDetailed, cwLastGame, cwOpponents, cwSummary :: Command

cwAddGame mess = do
	possible <- sqlQuery "SELECT id, tag, name FROM clans WHERE tag ILIKE ('%' || ? || '%')" [toSql opponent]
	case possible of
		[] -> Echo >>> opponent ++ ": Not found."

		[[id, _, name]] -> do
			unix <- fromMaybe <$> io getUnixTime <*> pure (mread $ firstWord timestamp)
			sqlTransaction $ sqlRun "INSERT INTO cw_games (clan, unix) VALUES (?, ?)"
				[id, toSql unix]
			[[gameid]] <- sqlQuery' "SELECT id FROM cw_games ORDER BY id DESC LIMIT 1" []
			Echo >>> "Game versus " ++ fromSql name ++ " added with id " ++ fromSql gameid ++ "."

		xs -> Echo >>> manyClans xs

	where (opponent, timestamp) = breakDrop isSpace mess

cwDelGame mess = do
	query <- sqlQuery' "SELECT clan FROM cw_games WHERE id = ?" [toSql id]
	case query of
		[[clan]] -> do
			sqlTransaction $ sqlRun "DELETE FROM cw_games WHERE id = ?" [toSql id]
			Echo >>> "Game ("++id++")" ++ fromSql clan ++ " successfully removed."

		_	-> Echo >>> "Game id ("++id++") not found."

	where id = firstWord mess

cwAddRound mess = case words mess of
	[id, map',[as,hs]] | okscore as && okscore hs -> let
		err _	= Echo >>> "Adding round Failed. Perhaps the id is incorrect?"
		try	= do
			sqlRun "INSERT INTO cw_rounds (cw_game, map, ascore, hscore) VALUES (?, ?, ?, ?)"
				[toSql id, toSql map', toSql as, toSql hs]
			[[clan]] <- sqlQuery' "SELECT name FROM cw_games JOIN clans ON clan = clans.id WHERE cw_game.id = ?" [toSql id]
			Echo >>> "Round added to ("++id++")"++fromSql clan++"."
		in sqlTransactionTry try err

	_	-> Error >>> "Error in syntax."

	where okscore x = x `elem` "wldn"

cwListGames mess = do
	[[num]]	<- sqlQuery' "SELECT COUNT(*) FROM cw_games" []
	let	num'	= fromSql num :: Int
		start	= inrange 0 num' $ (fromMaybe (num'-10) $ mread $ firstWord mess :: Int)
		end	= min num' (start + 10)
 	q <- sqlQuery "SELECT cw_games.id, name FROM cw_games JOIN clans ON clan = clans.id ORDER BY unix, id LIMIT 10 OFFSET ?" [toSql start]
	Echo >>> printf "Games %d-%d of %d: %s" start end num' (intercalate ", " (fmap format q))

	where	format = \[id, clan] -> "(" ++ fromSql id ++ ")" ++ fromSql clan
		inrange x y = max x . min y

game :: SqlValue -> RiverCom ()
game search = do
	q <- sqlQuery' "SELECT cw_games.id, name, unix FROM cw_games JOIN clans ON clan = clans.id WHERE cw_games.id = ?" [search]
	case q of
		[[id, clan, unix]] -> do
			scores	<- sqlQuery' "SELECT map, ascore, hscore FROM cw_rounds WHERE cw_game = ?" [id]
			now	<- io getUnixTime
			let	id'	= fromSql id :: Int
				maps'	= intercalate ", " $ fmap formatScore scores
				clan'	= fromSql clan :: String
				time	= (now - (fromSql unix)) // (60*60*24)
				Score tW tL tD	= sum $ sqlToScore scores
			Echo >>> printf "\STX(\STX%d\STX)%s:\STX %d days ago on: %s \STXRounds:\STX %d won, %d lost and %d draw."
				id' clan' time maps' tW tL tD

		_ -> Echo >>> "Id (" ++ (fromSql search) ++ "): Not found."

	where	formatScore = \[map, asc, hsc] -> fromSql map ++ "(" ++ [fromSql asc, fromSql hsc] ++ ")"
		sqlToScore xs = let f = toScore . fromSql in [a+h | [_, f -> Just a, f -> Just h] <- xs]

cwGame mess = game (toSql $ fromMaybe (0::Int) (mread mess))

cwLastGame _ = do
	q <- sqlQuery' "SELECT id FROM cw_games ORDER BY unix DESC LIMIT 1" []
	maybeL (Echo >>> "No games played.") (game . head) q


summary :: [(Int, Score)] -> String
summary lst = let
	games		= IM.fromListWith (+) lst
	Score gW gL gD	= foldl' gamecount 0 games
	Score tW tL tD	= sum games
	gTot		= IM.size games
	tTot		= tW + tL + tD
	in printf "\STX%d Games: %.1f%% won\STX (won: %d, lost: %d, draw: %d) || \STX%d Rounds: %.1f%% won\STX (won: %d, lost: %d, draw: %d)"
			gTot (ratio gW gTot) gW gL gD  tTot (ratio tW tTot) tW tL tD
	where
	gamecount (Score a b c) (Score w l _)
		| w > l		= Score (a+1) b c
		| w < l		= Score a (b+1) c
		| otherwise	= Score a b (c+1)

	ratio x tot = 100 * (fromIntegral x) / (fromIntegral tot) :: Double

cwSummary opponent
	| null opponent = do
		q <- sqlQuery "SELECT cw_game,ascore,hscore FROM cw_rounds" []
		Echo >>> (summary $ format $ q)

	| otherwise = do
		possible <- sqlQuery playedClans [toSql opponent]
		case possible of
			[] -> Echo >>> opponent ++ ": No games played."
			[[id, _, name]] -> do
				q <- sqlQuery "SELECT cw_game,ascore,hscore FROM cw_rounds JOIN cw_games ON cw_game = cw_games.id WHERE clan = ?" [id]
				Echo >>> view (fromSql name) (summary $ format q)
			xs -> Echo >>> manyClans xs

	where format xs = let f = toScore . fromSql in [(fromSql id, a + h) | [id, f -> Just a, f -> Just h] <- xs]

cwOpponents _ = do
	q <- sqlQuery "SELECT name FROM (SELECT DISTINCT ON (clan) * FROM cw_games JOIN clans ON clan = clans.id) tmp ORDER BY unix" []
	Echo >>> intercalate ", " (fmap (fromSql . head) q)

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

cwDetailed opponent
	| null opponent = do
		q <- sqlQuery "SELECT map,ascore,hscore FROM cw_rounds" []
		EchoM >>> detailed $ format $ q

	| otherwise = do
		possible <- sqlQuery playedClans [toSql opponent]
		case possible of
			[] -> Echo >>> opponent ++ ": No games played."
			[[id, _, _]] -> do
				q <- sqlQuery "SELECT map,ascore,hscore FROM cw_rounds JOIN cw_games ON cw_game = cw_games.id WHERE clan = ?" [id]
				EchoM >>> detailed $ format $ q
			xs -> Echo >>> manyClans xs

	where format xs = let f = toScore . fromSql in [(Nocase $ fromSql id, (a, h)) | [id, f -> Just a, f -> Just h] <- xs]

manyClans :: [[SqlValue]] -> String
manyClans xs = "Possible choices: " ++ (intercalate ", " $ fmap (\[_,x,_] -> fromSql x) xs)

playedClans :: String
playedClans = "SELECT DISTINCT ON (clan) clans.id, tag, name FROM clans JOIN cw_games ON clan = clans.id WHERE tag ILIKE ('%' || ? || '%')"
