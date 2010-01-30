module Module.Core (mdl) where
import Module
import qualified Data.Map as M
import Data.List
import Irc.OnEvent (updateConfig)

mdl :: Module x
mdl = Module
	{ modName	= "core"
	, modFinish	= return ()
	, modList	=
		[ ("commands"		, (comCommands	, 0	, Peon		, ""
			, "Lists all commands."))
		, ("help"		, (comHelp	, 0	, Peon		, "<command/alias>"
			, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
		, ("modulerestart"	, (comModuleRestart	, 1	, Master	, "<module>"
			, "Restart a module, or use * to restart every module."))
		, ("updateconfig"	, (comUpdateConfig	, 0	, Master	, ""
			, "Parse river.conf and update required settings. A modulerestart might be necessary for some changes to apply."))
		]
	, modInit	= do
		xs <- getActiveModules
		modify $ \x -> x { commands = M.fromList $ concatMap modList xs}
	}

comModuleRestart, comCommands, comHelp, comUpdateConfig :: Command x

comModuleRestart mess = do
	modules <- gets (moduleHook . hooks)
	case find ((arg==) . modName) modules of
		_ | arg == "*"	-> f =<< getActiveModules
		Just x		-> f [x]
		Nothing		-> Error >>> "No module matching \"" ++ arg ++ "\"."
	where
	arg = fmap toLower $ firstWord mess
	f xs = do
		state <- get
		put =<< (io $ execRiver (mapM_ (\x -> modFinish x >> modInit x ) xs) state)
		Echo >>> view "Modules restarted" (intercalate ", " (fmap modName xs))

comCommands arg' = do
	modules <- getActiveModules
	Echo >>> case find ((fmap toLower arg==) . modName) modules of
		Just x			-> format x
		Nothing | null arg	-> (unwords . fmap format) modules
		Nothing			-> view arg "No such module."
	where	arg = fmap toLower arg'
		format Module{modName, modList} = view modName (intercalate ", " (sort $ fmap fst modList))

comHelp mess
	| null mess = do
		k <- gets (comkey . config)
		Echo >>> "Use " ++ k ++ "commands or " ++ k ++ "aliases for a list of available functions."

	| otherwise = do
		commands	<- gets commands
		let arg		= map toLower $ firstWord mess
		case M.lookup arg commands of
			Nothing -> do
				query <- gets (aliasHook . hooks) >>= \x -> x arg
				Echo >>> case query of
					Nothing		-> view arg "Command or alias not found."
					Just (a, b)	-> "(alias) " ++ arg ++ " \STX->\STX " ++ a ++ " " ++ b
				Echo >>> view arg "Command or alias not found."

			Just (_,_,_,help, info)	-> Echo >>> "\STX" ++ arg ++ helpargs ++ ":\STX " ++ info
				where helpargs = (if not $ null help then " " else "") ++ help

comUpdateConfig _ = do
	path	<- (++"river.conf") <$> gets path
	newconf	<- getConfig <$> io (readFile path)
	case newconf of
		Left e -> Whisper >>> view "river.conf" e
		Right new -> do
			modify $ \x -> x{ config=new }
			sendM . Irc.OnEvent.updateConfig new =<< gets ircState
			Echo >>> "Configuration updated."
