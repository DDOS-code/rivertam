module ComCW (list) where
import Text.Printf
import System.Time
import System.Locale
import Database.HDBC

import CommandInterface
import Config
import Helpers

list :: CommandList
list =
	[ ("cw-summary"		, (withCW cwSummary	, 0	, Peon	, "(clan)"
		, "A summary of rounds played. (Use the argument for clan-filtering)"))
	, ("cw-opponents"	, (withCW cwOpponents	, 0	, Peon	, ""
		, "List every oppenent the clan has played against."))
	, ("cw-detailed"	, (withCW cwDetailed	, 0	, Peon	, ""
		, "Detailed stats about the clangames. (Use the argument for clan-filtering)"))
	, ("cw-lastgame"	, (withCW cwLast	, 0	, Peon	, ""
		, "Last clangame that was played."))
	--, ("cw-add"		, (comCWaddgame		, 3	, User	, "<clanname> <map> <score>"
	--	, "Add a clangame to the database. Example: \"ddos niveus wd\". For the last field: (w)on/(l)ost/(d)raw/(n)ot played, first aliens then humans."))
	]

type CWModule = String -> String -> [ClanGame] -> [String]

data ClanGame =	ClanGame {
			  date	:: Integer
			, clan
			, cgMap		:: String
			, score	:: TOTScore
			} deriving Eq

data TOTScore = TOTScore !Score !Score deriving (Eq)
data Score = Score !Int !Int !Int deriving (Eq, Show)

instance (Ord ClanGame) where
	compare a b = compare (date a) (date b)

instance (Show ClanGame) where
	show (ClanGame date clan cgMap score) =
		unwords [show date, clan, cgMap, show score]


instance (Show TOTScore) where
	show (TOTScore a h) = f a : f h : []
		where f x = case x of
			Score 1 0 0	-> 'w'
			Score 0 1 0	-> 'l'
			Score 0 0 1	-> 'd'
			_		-> 'n'

instance (Num TOTScore) where
	(TOTScore a1 b1) + (TOTScore a2 b2) =TOTScore (a1+a2) (b1+b2)
	abs		= undefined
	(*)		= undefined
	signum		= undefined
	fromInteger	= undefined

instance (Num Score) where
	(Score a1 b1 c1) + (Score a2 b2 c2) = Score (a1+a2) (b1+b2) (c1+c2)
	abs		= undefined
	(*)		= undefined
	signum		= undefined
	fromInteger	= undefined


roundwld :: Score -> String
roundwld (Score w l _)
	| w > l		= "victory"
	| w < l		= "defeat"
	| otherwise	= "draw"

{-
comCWaddgame :: Command
comCWaddgame _ mess Info {filePath, echo} _ = do
	TOD unixs _ 	<- getClockTime
	-- This one is pretty ugly, but it was The Easy Way (tm)
	case parse clanGameEntry "" (show unixs ++" "++ mess) of
		Left _	-> echo $ "Error in syntax."
		Right a	-> do
			appendFile (filePath++clanFile) $ show a ++ "\n"
			echo $ "Clangame added."
			-}

formatSql [fromSql -> date, fromSql -> clan, fromSql -> map', fromSql -> [ff -> Just a, ff -> Just h]]
	= ClanGame date clan map' (TOTScore a h)
formatSql _ = error "dala"

ff c = case c of
		'w'	-> Just $ Score 1 0 0
		'l'	-> Just $ Score 0 1 0
		'd'	-> Just $ Score 0 0 1
		'n'	-> Just $ Score 0 0 0
		_	-> Nothing

withCW :: CWModule -> Command
withCW func nick mess Info{echo} ComState{conn} = do
	query	<- fmap formatSql `fmap` quickQuery conn "SELECT date,clan,map,score FROM cw" []
	case query of
		[]	-> echo $ "No clangames played yet."
		cont	-> mapM_ echo (func nick mess cont)

cwSummary, cwDetailed, cwOpponents, cwLast :: CWModule

cwSummary _ mess c
	| tot /= 0	= [printf "\STX%s\STX: rounds: %d | won: %d / lost: %d / draw: %d | %.1f%% won"
				name tot tW tL tD winratio]
	| otherwise	= [printf "\STX%s\STX: not in my database." name]
	where
	arg		= takeWhile (not . isSpace) mess
	(clans, name) = if null arg then (c, "Total") else
				(filter (\x -> clan x =|= arg) c, arg)
	TOTScore a h	= summary $ map score clans
	Score tW tL tD	= a + h
	tot		= tW + tL + tD
	winratio	= 100.0 * (fromIntegral tW) / (fromIntegral tot) :: Double
	summary = foldl' (+) (TOTScore (Score 0 0 0) (Score 0 0 0))

cwDetailed _ mess c
	| not $ null maps	= header : map format merged ++ [footer]
	| otherwise		= [printf "\STX%s\STX: not in my database." arg]
	where	arg	= takeWhile (not . isSpace) mess
		maps	= map (\x -> (cgMap x, score x)) $ (if null arg then id else filter (\x -> clan x=|= arg)) c
		merged	= mergemaps maps
		TOTScore (Score taW taL taD) (Score thW thL thD) = foldl1' (+) (map snd merged)

		header	= "\STXMap\ETX4           aW  aL  aD\ETX12      hW  hL  hD"
		footer	= printf "\STXTotal\ETX4         %2d  %2d  %2d\ETX12      %2d  %2d  %2d" taW taL taD thW thL thD
		format (m, TOTScore (Score aW aL aD) (Score hW hL hD)) =
			printf "%-13s\ETX4 %2d  %2d  %2d\ETX12      %2d  %2d  %2d" m aW aL aD hW hL hD


mergemaps :: [(String, TOTScore)] -> [(String, TOTScore)]
mergemaps maps = merge . mapswithscore . uniquemaps $ maps
	where	uniquemaps	= nubBy (=|=) . map fst
		mapswithscore	= map (\str -> (str, map snd . filter (\(x,_)->x=|=str) $ maps))
		merge		= map (\(a, b) -> (a, foldl1' (+) b))


cwOpponents _ _ = (:[]) . intercalate ", "  . nubBy (=|=) . map clan

cwLast _ _ c = [printf "Last clangame was a %s versus \STX%s\STX on %s, %s."
				outcome clan cgMap timestr]
	where
	ClanGame {date, clan, cgMap, score = (TOTScore a h)} = maximum c
	timestr	= formatCalendarTime defaultTimeLocale "%c" (toUTCTime (TOD date 0))
	outcome	= roundwld $ a + h

{-
formatClanFile :: String -> Either ParseError [ClanGame]
formatClanFile = parse (sepEndBy clanGameEntry spaces) ""

clanGameEntry :: GenParser Char st ClanGame
clanGameEntry = do
	date	<- decimal (makeTokenParser haskellDef)
	spaceSep
	clan	<- anyToSpace
	spaceSep
	cmap	<- anyToSpace
	spaceSep
	[a,h]	<- anyToSpace
	spaces
	a' <- f a
	h' <- f h
	return $ ClanGame date clan cmap (TOTScore a' h')
	where f c = case c of
		'w'	-> return $ Score 1 0 0
		'l'	-> return $ Score 0 1 0
		'd'	-> return $ Score 0 0 1
		'n'	-> return $ Score 0 0 0
		_	-> fail "Score formatting error"

spaceSep, anyToSpace :: GenParser Char st String
spaceSep = many1 space
anyToSpace = many1 $ satisfy (not . isSpace)
-}
