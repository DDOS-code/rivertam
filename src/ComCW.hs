module ComCW(list) where
import Text.Printf
import Text.ParserCombinators.Parsec hiding (try)
import System.IO.Error
import System.Time
import System.Locale
import Prelude hiding (catch)
import Text.Read hiding (lift)

import Send
import Config
import RiverState hiding (get)
import Helpers

list :: CommandList
list =
	[ ("cw-summary"		, (comCWsummary		, 0	, Peon	, "(clan)"
		, "A summary of rounds played. (Use the argument for clan-filtering)"))
	, ("cw-opponents"	, (comCWopponents	, 0	, Peon	, ""
		, "List every oppenent the clan has played against."))
	, ("cw-detailed"	, (comCWdetailed	, 0	, Peon	, ""
		, "Detailed stats about the clangames. (Use the argument for clan-filtering)"))
	, ("cw-lastgame"	, (comCWLast		, 0	, Peon	, ""
		, "Last clangame that was played."))
	, ("cw-add"		, (comCWaddgame		, 3	, User	, "<clanname> <map> <score>"
		, "Add a clangame to the database. Example: '\"ddos\" \"niveus\" wd'. For the last field: (w)on/(l)ost/(d)raw/(n)ot played, first aliens then humans."))
	]

clanFile :: String
clanFile = "clanstat.conf"

data ClanGame =	ClanGame {
			  cgDate	:: Integer
			, cgClan
			, cgMap		:: String
			, cgScore	:: TOTScore
			} deriving Eq

data TOTScore = TOTScore !Score !Score deriving (Eq)
data Score = Score !Int !Int !Int deriving (Eq, Show)

instance (Ord ClanGame) where
	compare a b = compare (cgDate a) (cgDate b)

instance (Show ClanGame) where
	show (ClanGame cgDate cgClan cgMap cgScore) =
		unwords [show cgDate, cgClan, cgMap, show cgScore]


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



comCWsummary, comCWdetailed, comCWaddgame, comCWLast, comCWopponents :: Command

comCWsummary (_, chan, mess_) = withClanFile chan $ \claninfo -> do
	let	arg		= takeWhile (not . isSpace) mess_
		(!clans, !name) = if null arg then (claninfo, "Total") else
					(filter (\x -> cgClan x =|= arg) claninfo, arg)
		TOTScore a h	= summary $ map cgScore clans
		Score tW tL tD	= a + h
		tot		= tW + tL + tD
		winratio	= 100.0 * (fromIntegral tW) / (fromIntegral tot) :: Double

	Msg chan >>> if tot /= 0
		then printf "\STX%s\STX: rounds: %d | won: %d / lost: %d / draw: %d | %.1f%% won"
				name tot tW tL tD winratio
		else printf "\STX%s\STX: not in my database." name

		where summary = foldl' (+) (TOTScore (Score 0 0 0) (Score 0 0 0))



comCWopponents (_, chan, _) = withClanFile chan $ \claninfo -> do
	Msg chan >>> case nubBy (=|=) . map cgClan $ claninfo of
		[]	-> "No opponents found."
		a	-> intercalate ", " a


comCWdetailed (_, chan, mess_) = withClanFile chan $ \clanfile -> do
	case clanfile of
		[] -> Msg chan >>> "No maps played."
		info -> do
			let	arg	= takeWhile (not . isSpace) mess_
				maps	= map (\x -> (cgMap x, cgScore x)) $ (if null arg then id else filter (\x -> cgClan x=|= arg)) info
				merged	= mergemaps maps
				TOTScore (Score taW taL taD) (Score thW thL thD) = foldl1' (+) (map snd merged)

			if not $ null maps then do
				Msg chan >>> "\STXMap\ETX4           aW  aL  aD\ETX12      hW  hL  hD"
				forM_ merged $ \(m, TOTScore (Score aW aL aD) (Score hW hL hD)) ->
					Msg chan >>> printf "%-13s\ETX4 %2d  %2d  %2d\ETX12      %2d  %2d  %2d" m aW aL aD hW hL hD
				Msg chan >>> printf "\STXTotal\ETX4         %2d  %2d  %2d\ETX12      %2d  %2d  %2d" taW taL taD thW thL thD
			 else do
				Msg chan >>> printf "\STX%s\STX: not in my database." arg

mergemaps :: [(String, TOTScore)] -> [(String, TOTScore)]
mergemaps maps = merge . mapswithscore . uniquemaps $ maps
	where	uniquemaps	= nubBy (=|=) . map fst
		mapswithscore	= map (\str -> (str, map snd . filter (\(x,_)->x=|=str) $ maps))
		merge		= map (\(a, b) -> (a, foldl1' (+) b))


comCWLast (_, chan, _) = withClanFile chan $ \claninfo -> do
	if null claninfo then Msg chan >>> "No clangames played." else do
		let	ClanGame {cgDate, cgClan, cgMap} = maximum claninfo
			date	= toUTCTime (TOD cgDate 0)
			timestr	= formatCalendarTime defaultTimeLocale
		Msg chan >>> timestr ("Last clangame was versus \STX"++cgClan++"\STX on "++cgMap++", %c.") date

comCWaddgame (_, chan, mess) = do
	TOD unixs _ 	<- lift $ getClockTime
	case parse clanGameEntry "" (show unixs ++" "++ mess) of
		Left e	-> do	Msg chan >>> "Error in syntax."
				lift $ print e
				lift $ print mess
		Right a	-> do
			rivConfDir	<- gets rivConfDir
			lift $ appendFile (rivConfDir++clanFile) $ show a ++ "\n"
			Msg chan >>> "Clangame added."


withClanFile :: String -> ([ClanGame] -> RiverState) -> RiverState
withClanFile chan func = do
	rivConfDir	<- gets rivConfDir
	test		<- lift $ try $ getstuff $ rivConfDir++clanFile
	case test of
		Left _ -> do
			Msg chan >>> "Clan-database not found."
		Right (hdl, cont) -> do
			case formatClanFile cont of
				Right a	-> func a
				Left e	-> Msg chan >>> "Error in the clan-database: "
							 ++ show (errorPos e)
			lift $ hClose hdl

	where getstuff fx = do
		hdl	<- openFile fx ReadMode
		cont	<- hGetContents hdl
		return (hdl, cont)

formatClanFile :: String -> Either ParseError [ClanGame]
formatClanFile = parse (sepEndBy clanGameEntry spaces) ""

clanGameEntry :: GenParser Char st ClanGame
clanGameEntry = do
	date	<- takeToSpace digit
	clan	<- anyToSpace
	cmap	<- anyToSpace
	[a,h]	<- anyToSpace
	a' <- f a
	h' <- f h
	return $ ClanGame (read date) clan cmap (TOTScore a' h')
	where f c = case c of
		'w'	-> return $ Score 1 0 0
		'l'	-> return $ Score 0 1 0
		'd'	-> return $ Score 0 0 1
		'n'	-> return $ Score 0 0 0
		_	-> fail "Score formatting error"

anyToSpace :: GenParser Char st String
anyToSpace = spaces >> many1 (satisfy (not . isSpace))

takeToSpace :: GenParser Char st a -> GenParser Char st [a]
takeToSpace x = spaces >> many1 x


