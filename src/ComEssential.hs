module ComEssential (wee) where
import CommandInterface
import System.Info
import Data.Version
import qualified Data.Map as M
import Data.List
import IRC
import ComAlias (fetchAlias)

wee :: Module
wee = Module
	{ modName	= "essential"
	, modInit	= return ()
	, modFinish	= return ()
	, modList	=
		[ ("about"		, (comAbout	, 0	, Peon		, ""
			, "Brief info about the bot."))
		, ("echo"		, (comEcho	, 1	, Peon		, "<<message>>"
			, "Echoes back whatever argumet you supply. \"%s\" will get replaced with your nick. Good for creating aliases. /me is supported."))
		, ("pingall"		, (comPingall	, 0	, User		, ""
			, "Will echo back a list of every user in the channel."))
		, ("source"		, (comSource	, 0	, Peon		, ""
			, "Displays git url."))
		, ("uptime"		, (comUptime		, 0	, Peon		, ""
			, "Displays uptime (obviously)."))
		, ("commands"		, (comCommands	, 0	, Peon		, ""
			, "Lists all commands."))
		, ("help"		, (comHelp	, 0	, Peon		, "<command/alias>"
			, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
		, ("modulerestart"	, (comModuleRestart , 1	, Master	, "<module>"
			, "Restart a module, or use * to restart every module."))
		]
	}

comEcho, comAbout, comSource, comPingall, comUptime, comCommands, comHelp, comModuleRestart :: Command

comEcho mess = do
	(Name (Nocase nick) _ _) <- asks userName
	Echo >>> replace "%s" nick mess

comAbout _ = Echo >>>
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ = Echo >>> "git clone git://git.mercenariesguild.net/rivertam.git"

comPingall _ = do
	users <- getUserList
	case M.keys users of
		[]	-> Error >>> "No users found."
		a	-> mapM_ (Echo >>>) $ neatList $ fmap recase a

	-- Max length for an irc message is 512 chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x

comUptime _ = (Echo >>>) =<< format <$> gets initTime <*> io getUnixTime
	where
	format started now = "Running for " ++ formatTime sec ++ ". e-shoes: " ++ eshoes
		where
		sec	= fromInteger (now-started) ::Int
		day	= sec // 86400 + 1 -- She must have at least 1 pair of shoes, thus the +1 :)
		eshoes	= "|" ++ (replicate day '.') ++ "|"

comCommands _ = do
	modules <- asks modulesI
	Echo >>> "Commands are: " ++ unwords (fmap format modules)
	where format Module{modName, modList} = view modName (intercalate ", " (sort $ fmap fst modList))

comHelp mess
	| null mess = do
		k <- gets (comkey . config)
		Echo >>> "Use "++k++"commands or "++k++"aliases for a list of available functions."

	| otherwise = asks modulesI >>= \m -> case lookup arg (concatMap modList m) of
		Nothing -> do
			query <- ComAlias.fetchAlias arg
			case query of
				Nothing	-> Error >>> "Command or alias not found."
				Just a	-> Echo >>> "(alias) " ++ arg ++ " \STX->\STX " ++ a

		Just (_,_,_,help, info)	-> Echo >>> "\STX" ++ arg ++ helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
	where arg = firstWord mess

comModuleRestart mess = do
	modulesI <- asks modulesI
	case filter ((arg==) . modName) modulesI of
		[x]		-> f [x]
		[] | arg == "*"	-> f modulesI
		_		-> Error >>> "No module matching \"" ++ arg ++ "\"."
	where
	arg = fmap toLower $ firstWord mess
	f xs = do
		state <- get
		put =<< (io $ execRiver (mapM_ (\x -> modFinish x >> modInit x ) xs) state)
		Echo >>> view "Modules restarted" (intercalate ", " (fmap modName xs))

