module Commands (command) where
import Data.Map(Map)
import qualified Data.Map as M
import Text.Printf
import System.Info
import Data.Version
import Prelude hiding (min)

import RiverState
import Config
import Helpers

data ComResponse = Msg !String | Notice !String

{-

	#ifndef nocw
	import qualified ComCW
	#endif
	#ifndef noflame
	import qualified ComFlameLove
	#endif
	#ifndef notrem
	import qualified ComTrem
	#endif
-}
modules :: CommandList
modules = cListEssential
	{- #ifndef nocw
		++ ComCW.list
	#endif
	#ifndef noflame
		++ ComFlameLove.list
	#endif
	#ifndef notrem
		++ ComTrem.list
	#endif
-}
cList :: CommandList
cList = sortBy (\(a, _) (b, _) -> compare a b) $ modules

cListMap :: Map String CommandInfo
cListMap = M.fromList cList

cListEssential :: CommandList
{-cListEssential =
	[ ("help"		, (comHelp	, 0	, Peon		, "(command)"
		, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
	, ("about"		, (comAbout	, 0	, Peon		, ""
		, "Brief info about the bot."))
	, ("uptime"		, (comUptime	, 0	, Peon		, ""
		, "Displays uptime (obviously)."))
	, ("moo"		, (comMoo	, 0	, Peon		, "((string))"
		, "Test function, will echo back the string."))
	, ("pingall"		, (comPingall	, 0	, User		, ""
		, "Will echo back a list of every user in the channel."))
	, ("alias"		, (comAlias	, 0	, Peon		, "(alias-key)"
		, "List of the current aliases, or with an argument expand the alias."))
	, ("clear"		, (comClear	, 0	, Master	, ""
		, "Clear the sender-queue."))
	, ("reparse"		, (comReparse	, 0	, Master	, ""
		, "Reparse the config file."))
	, ("source"		, (comSource	, 0	, Peon		, ""
		, "Displays git url."))
	]-}


--comHelp, comAbout, comMoo, comPingall, comAlias,  comUptime, comClear, comReparse, comSource :: Command
command :: Access -> IO ()
command accesslevel (nick, chan, mess) = do
	when ((not $ null fname) && accesslevel >= Peon) $
		case M.lookup fname cListMap of
			Nothing	->
				Notice nick >>> "\STX"++fname++":\STX Command not found."
			Just (_,_,access,_,_) | accesslevel < access ->
				Msg chan >>> "\STX"++fname++":\STX "++show access++"-access or higher needed."
			Just (_,args,_,help,_) | (length $ words fargs) < args ->
				Msg chan >>> "Missing arguments, usage: "++fname++" "++help
			Just (func, _,_,_,_) ->
				func (nick, chan, fargs)

	where	(a0, aE)	= break isSpace mess
		fname		= map toLower a0
		fargs		= stripw aE


comMoo (nick, chan, mess) = Msg chan >>> "Moo Moo, "++nick++": "++mess
{-
comHelp (nick, chan, mess)
	| null mess	= do
		Config {comkey} <- gets config
		Msg chan >>> "Commands are (key: "++comkey++"): " ++ (intercalate ", " . map fst $ cList)
	| otherwise	= case M.lookup arg cListMap of
		Just (_,_,_,help,info)	-> Msg chan >>> "\STX" ++ arg ++  helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
		Nothing			-> Notice nick >>> "Sorry, I don't know the command \""++arg++"\""
	where arg = head $ words mess

comAbout (_, chan, _) =
	Msg chan >>> "\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
		++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comUptime (_, chan, _) = do
	started	<- gets rivUptime
	now	<- lift $ getMicroTime
	let	sec			= fromInteger $ (now-started) // 1000000 :: Int -- pico -> seconds
		(min, hour, day)	= (sec//60, min//60, hour//24)
		(fsec, fmin, fhour)	= (sec%60, min%60, hour%24)
		(ds, hs, ms, ss)	= (dds "day" day, dds "hour" fhour, dds "minute" fmin, dds "second" fsec)
		eshoes			= "|" ++ replicate ((day+1)*2) '.' ++ "|"
		str			= printf "Running for %d %s, %d %s, %d %s and %d %s. e-shoes: %s" day ds fhour hs fmin ms fsec ss eshoes
	Msg chan >>> str
	where dds m s = if s /= 1 then m++"s" else m



comAlias (_, chan, args) = do
	Config {alias}	<- gets config
	Msg chan >>> if null args
		then "Aliases are: " ++ intercalate ", "  [x | (x, _) <- M.toList alias]
		else let arg = head . words $ args in arg++" \STX->\STX " ++ fromMaybe "No such alias." (M.lookup arg alias)

comPingall (_, chan, _) = do
	rivMap		<- gets rivMap
	let users 	= maybe [] M.toList $ M.lookup (map toLower chan) rivMap
	Msg chan >>> unwords (map fst users)

comClear _ = (lift . atomically . clearSender) =<< gets rivSender

comReparse (_, chan, _) = do
	rivConfDir	<- gets rivConfDir
	oldc		<- gets config
	newc		<- lift $! getConfig `liftM` readFileStrict (rivConfDir++"river.conf")
	case newc of
		Right a | oldc /= a -> do
			modify (\x -> x {config=a})
			Msg chan >>> "reparse: Config updated successfully."
			rivNick	<- gets rivNick

			unless (rivNick =|= nick a) $ do
				Nick >>> nick a

			when (channels oldc /= channels a) $ do
				let	oldchans	= channels oldc \\ channels a
					newchans	= channels a \\ channels oldc
				mapM_ (\(c, _) -> Part c >>> "over and out") oldchans
				mapM_ (\(c, pass) -> Join c >>> pass) newchans

		Right _ ->
			Msg chan >>> "reparse: Internal config already up to date."

		Left e ->
			Msg chan >>> "reparse: Using old config, " ++ e

comSource (_, chan, _) = Msg chan >>> "git clone git://git.mercenariesguild.net/rivertam.git"
-}
