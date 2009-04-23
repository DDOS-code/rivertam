module ComCW(list) where
import Text.Printf
import System.IO.Error
import Prelude hiding (catch)

import Send
import Config
import RiverState
import Helpers

list :: CommandList
list =
	[ ("cw-summary"		, (comCWsummary		, 0	, Peon	, "(clan)"
		, "A summary of rounds played. (Use the argument for clan-filtering)"))
	, ("cw-listopponents"	, (comCWopponents	, 0	, Peon	, ""
		, "List every oppenent the clan has played against."))
	, ("cw-detailed"	, (comCWdetailed	, 0	, Peon	, ""
		, "Detailed stats about the clangames. (Use the argument for clan-filtering)"))
	, ("cw-lastgame"	, (comCWLast		, 0	, Peon	, ""
		, "Last clangame that was played."))
	, ("cw-addgame"		, (comCWaddgame		, 4	, User	, "<date> <clanname> <map> <score>"
		, "Add a clangame to the database. Example: '2009-01-01 ddos niveus wd'. For the last field: (w)on/(l)ost/(d)raw/(-)not played, first aliens then humans."))
	]

clanFile = "clanstat.conf"

type WLD = (Int, Int, Int)
type ClanGame = (String, String, String, (WLD, WLD))

summary :: [ClanGame] -> (WLD, WLD)
summary strs = func strs (0,0,0) (0,0,0) where
	func [] a h					= (a, h)
	func ((_,_,_,(alien,human)):xs) ac hc	= func xs (alien+ac) (human+hc)


comCWsummary, comCWdetailed, comCWaddgame, comCWLast, comCWopponents :: Command

comCWsummary (_, chan, mess) = do
	claninfo <- getClanFile
	let	arg = head $ words mess
		(clans, name) = if length mess >= 1
			then (filter (\(_,clan,_,_) -> clan =|= arg) claninfo, arg)
			else (claninfo, "Total")
		(a, h)	= summary clans
		(tW, tL, tD) = a + h
		tot = tW + tL + tD
		winratio = 100.0 * (fromIntegral tW) / (fromIntegral tot) :: Double

	Msg chan >>> if tot /= 0
		then printf "\STX%s\STX: rounds: %d | won: %d / lost: %d / draw: %d | %.1f%% won" name tot tW tL tD winratio
		else printf "\STX%s\STX: not in my database." name

comCWopponents (_, chan, _) = do
	claninfo <- getClanFile
	let clans = case nub [x | (_,x,_,_) <- claninfo] of
		[]	-> "No opponents found."
		a	-> unsplit ", " a
	Msg chan >>> clans


comCWdetailed (_, chan, mess) = do
	clanfile <- getClanFile
	case clanfile of
		[] -> Msg chan >>> "No maps played."
		info -> do
			let	arg		= head $ words mess
				maps		= if not $ null mess
							then [(a,b) | (_,clan,a,b) <- info, clan =|= arg]
							else [(a,b) | (_,_,a,b) <- info]
				merged		= mergemaps maps
				((taW, taL, taD), (thW, thL, thD)) = foldl1' (+) (map snd merged)

			if not $ null maps then do
				Msg chan >>> "\STXMap\ETX4           aW  aL  aD\ETX12      hW  hL  hD"
				forM_ merged $ \(m, ((aW, aL, aD), (hW, hL, hD))) -> Msg chan >>> printf "%-13s\ETX4 %2d  %2d  %2d\ETX12      %2d  %2d  %2d" m aW aL aD hW hL hD
				Msg chan >>> printf "\STXTotal\ETX4         %2d  %2d  %2d\ETX12      %2d  %2d  %2d" taW taL taD thW thL thD
			 else do
				Msg chan >>> printf "\STX%s\STX: not in my database." arg

mergemaps :: [(String, (WLD, WLD))] -> [(String, (WLD, WLD))]
mergemaps maps = merge . mapswithscore . uniquemaps $ maps
	where	uniquemaps	= nub . map fst
		mapswithscore	= map (\str -> (str, map snd . filter (\(x,_)->x=|=str) $ maps))
		merge		= map (\(a, b) -> (a, foldl1' (+) b))


comCWLast (_, chan, _) = do
	info <- getClanFile
	let (date, clan,_,_) = last info
	Msg chan >>> printf "Last clangame was versus \STX%s\STX, the %s." clan date

comCWaddgame (_, chan, mess) =
	let sargs	= words mess in
	if length sargs == 4 && length (sargs!!0) == 10 && length (sargs!!3) == 2
		then do
			rivConfDir <- gets rivConfDir
			lift $ appendFile (rivConfDir++clanFile) $ unsplit "\t" sargs ++ "\n"
			Msg chan >>> "Clangame added."
		else Msg chan >>> "Error in syntax."

getClanFile :: StateT River IO [ClanGame]
getClanFile = do
	rivConfDir	<- gets rivConfDir
	contents	<- lift $ (readFile $ rivConfDir++clanFile) `catch` (\_-> return [])
	return $ formatClanFile contents

formatClanFile :: String -> [ClanGame]
formatClanFile = clans . splitlines
	where
	clans = map (listToTuple4 . words)
	addwin :: Char -> (Int, Int, Int)
	addwin l = case l of
		'w'	-> (1, 0, 0)
		'l'	-> (0, 1, 0)
		'd'	-> (0, 0, 1)
		_	-> (0, 0, 0)

	listToTuple4 [a,b,c,(al:hu:[])]	= (a,b,c,(addwin al, addwin hu))
	listToTuple4 _			= ("0000-00-00", "error", "error", ((0,0,1), (0,0,1)))
