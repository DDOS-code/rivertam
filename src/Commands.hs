module Commands (command, matchnuh) where
import Data.Map(Map)
import qualified Data.Map as M
import Text.Printf
import System.Info
import Data.Version

import Send
import RiverState
import Config
import Helpers

import qualified ComCW
import qualified ComFlameLove
import qualified ComTrem

modules :: CommandList
modules = ComTrem.list ++ ComFlameLove.list ++ ComCW.list

cList :: CommandList
cList = sortBy (\(a, _) (b, _) -> compare a b) $ cListEssential ++ modules

cListMap :: Map String CommandInfo
cListMap = M.fromList cList

cListEssential :: CommandList
cListEssential =
	[ ("help"		, (comHelp		, 0	, Normal	, "(command)"
		, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
	, ("about"		, (comAbout		, 0	, Normal	, ""
		, "Brief info about the bot."))
	, ("uptime"		, (comUptime		, 0	, Normal	, ""
		, "Shows the uptime (obviously)."))
	, ("moo"		, (comMoo		, 0	, Normal	, "((string))"
		, "Test function, will echo back the string."))
	, ("pingall"		, (comPingall		, 0	, Master	, ""
		, "Will echo back a list of every user in the channel."))
	, ("alias"		, (comAlias		, 0	, Normal	, "(alias-key)"
		, "List of the current aliases, or with an argument expand the alias."))
	, ("clear"		, (comClear		, 0	, Master	, ""
		, "Clear the sender-queue."))
	, ("reparse"		, (comReparse		, 0	, Master	, ""
		, "Reparse the config file."))
	]


comHelp, comAbout, comMoo, comPingall, comAlias,  comUptime, comClear, comReparse :: Command
command :: From -> RiverState
command (nuh@(nick,_,_), chan, mess) = do
	rivMap			<- gets rivMap
	Config {access=macc}	<- gets config

	let	(a0, aE)	= break isSpace mess
		fname		= map toLower a0
		fargs		= stripw aE
		nickl		= map toLower nick
		chanl		= map toLower chan
		haveaccess	= fromMaybe Normal $ (M.lookup nickl =<< M.lookup chanl rivMap)

	when (not $ null fname) $
		case M.lookup fname cListMap of
			Nothing	->
				Notice nick >>> "\STX"++fname++":\STX Command not found."
			Just (_,_,access,_,_) | haveaccess < access && not (matchnuh macc nuh) ->
				Msg chan >>> "\STX"++fname++":\STX "++show access++"-access or higher needed."
			Just (_,args,_,help,_) | (length $ words fargs) < args ->
				Msg chan >>> "Missing arguments, usage: "++fname++" "++help
			Just (func, _,access,_,_) ->
				func (nick, chan, fargs)

comHelp (nick, chan, mess)
	| null mess	= let coms = foldl1' (\a b -> a++", "++b) $ map fst cList in
		Msg chan >>> "Available commands: " ++ coms
	| otherwise	= case M.lookup arg cListMap of
		Just (_,_,_,help,info)	-> Msg chan >>> "\STX" ++ arg ++ " " ++ help ++ ":\STX " ++ info
		Nothing			-> Notice nick >>> "Sorry, i don't know the command \""++arg++"\""
	where arg = head $ words mess

comAbout (_, chan, _) =
	Msg chan >>> "\STXriver-tam\STX, written by Christoffer Öjeling aka \"Cadynum\". Running on "
		++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ ". "
		++ "Get the source: \"git clone git://git.mercenariesguild.net/rivertam.git\""


comUptime (_, chan, _) = do
	started	<- gets rivUptime
	now	<- lift $ getMicroTime
	let	sec			= fromInteger $ (now-started) // 1000000 :: Int -- pico -> seconds
		(min, hour, day)	= (sec//60, min//60, hour//24)
		(fsec, fmin, fhour)	= (sec%60, min%60, hour%24)
		(ds, hs, ms, ss)	= (dds "day" day, dds "hour" fhour, dds "minute" fmin, dds "second" fsec)
		epenis			= "|" ++ concat (replicate (day+1) "..") ++ "|"
		str			= printf "Running for %d %s, %d %s, %d %s and %d %s. e-shoes: %s" day ds fhour hs fmin ms fsec ss epenis
	Msg chan >>> str
	where dds m s = if s /= 1 then m++"s" else m

comMoo (nick, chan, mess) = Msg chan >>> "Moo Moo, "++nick++": "++mess

comAlias (_, chan, args) = do
	Config {alias}	<- gets config
	Msg chan >>> if null args
		then "Available aliases: " ++ unwords  [x | (x, _) <- M.toList alias]
		else let arg = head . words $ args in arg++" \STX->\STX " ++ (maybe "No such alias." id (M.lookup arg alias))

comPingall (nick, chan, _) = do
	rivMap		<- gets rivMap
	let users 	= maybe [] M.toList $ M.lookup (map toLower chan) rivMap
	Msg chan >>> unwords (map fst users)

comClear _ = (lift . clearSender) =<< gets rivSender


comReparse (_, chan, _) = do
	rivConfDir	<- gets rivConfDir
	oldc		<- gets config
	newc		<- lift $! getConfig `liftM` readFile (rivConfDir++"river.conf")
	case newc of
		Right a | oldc /= a -> do
			modify (\x -> x {config=a})
			Msg chan >>> "reparse: Config updated successfully."

			when (channels oldc /= channels a) $ do
				let	oldchans	= channels oldc \\ channels a
					newchans	= channels a \\ channels oldc
				mapM_ (\(chan, pass) -> Part chan >>> "over and out") oldchans
				mapM_ (\(chan, pass) -> Join chan >>> pass) newchans

			when (nick oldc /= nick a) $
				Nick >>> nick a
		Right _ ->
			Msg chan >>> "reparse: Internal config already up to date."

		Left e ->
			Msg chan >>> "reparse: Using old config, " ++ e

matchnuh :: (String, String, String) -> (String, String, String) -> Bool
matchnuh (a1, a2, a3) (b1, b2, b3) = mi a1 b1 && mi a2 b2 && mi a3 b3
	where	mi []	_	= True
		mi "*"	_	= True
		mi x	y	= map toLower x == map toLower y
