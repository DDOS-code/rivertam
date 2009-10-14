module Command (
	command, commandInit, commandFinish
) where
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Exception
import IRC

import CommandInterface
import qualified ComEssential
import qualified ComAlias
import qualified ComQuotes
import qualified ComMemos
import qualified ComClans
import qualified ComCW
import qualified ComTrem
import qualified ComTremRelay

modulesL :: [Module]
modulesL = [mdl, ComEssential.mdl, ComAlias.m, ComQuotes.m, ComMemos.m, ComClans.m, ComCW.m, ComTrem.m, ComTremRelay.mdl]

commandInit, commandFinish :: River CState ()
commandInit = mapM_ modInit =<< getModules
commandFinish = mapM_ modFinish =<< getModules

command :: Nocase -> Access -> Nocase -> String -> String -> River CState ()
command channel access nick domain mess = when (not $ null fname || access < Peon) $ do
	start	<- io getMicroTime
	let info = Info {userAccess=access, channel, commandName = fname
			, nickName=nick, domain}
	state 	<- get
	cond	<- io $ catches (Right . snd <$> runRiverCom (tryCommand fargs) info state)
		[ Handler (\(e :: ArithException)	-> ex e)
		, Handler (\(e :: PatternMatchFail)	-> ex e)
		, Handler (\(e :: ErrorCall)		-> ex e)
		, Handler (\(e :: IOException)		-> ex e)
		, Handler (\(e :: ArithException)	-> ex e)
		, Handler (\(e :: SqlError)		-> ex e)
		]
	case cond of
		Left e	-> trace e >> (send $ Msg channel $ view fname "Exception raised!")
		Right s	-> put s
	end	<- io getMicroTime
	echo $ "Command " ++ fname ++ " time: " ++ show ((end-start) // 1000) ++ "ms"
	where
	(a0, fargs)	= breakDrop isSpace mess
	fname		= map toLower a0
	ex :: (Monad m, Show s) => s -> m (Either String a)
	ex = return . Left . show

tryCommand :: String -> RiverCom ()
tryCommand mess = do
	commands	<- getsCom commands
	name		<- asks commandName
	case M.lookup name commands of
		Nothing	-> do
			c <- ComAlias.fetchAlias name
			case c of
				Nothing	-> Error >>> "Command or alias not found."
				Just (name2, mess2) ->
					case M.lookup name2 commands of
						Nothing -> Error >>> "Alias Error."
						Just _ | isInfixOf "%a" mess2 && null mess ->
							Error >>> "The alias requires an argument."
						Just a -> doCommand a (replace "%a" mess mess2)
		Just a -> doCommand a mess

doCommand :: CommandInfo -> String -> RiverCom ()
doCommand (f, args, access, help, _) fargs = do
	userAccess <- asks userAccess
	case True of
		_ | userAccess < access ->
			Error >>> show access ++ "-access or higher needed."
		_ | not (atLeastLen args $ words fargs) ->
			Error >>> "Missing arguments. Usage: " ++ help
		_ -> f fargs

-- Core Modules ------------------------------------------------------------------------------------

mdl :: Module
mdl = Module
	{ modName	= "core"
	, modFinish	= return ()
	, modList	=
		[ ("commands"		, (comCommands	, 0	, Peon		, ""
			, "Lists all commands."))
		, ("help"		, (comHelp	, 0	, Peon		, "<command/alias>"
			, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
		, ("modulerestart"	, (comModuleRestart , 1	, Master	, "<module>"
			, "Restart a module, or use * to restart every module."))
		]
	, modInit	= do
		xs <- getModules
		modifyCom $ \x -> x { commands = M.fromList $ concatMap modList xs}
	}

comModuleRestart, comCommands, comHelp :: Command

comModuleRestart mess = do
	case find ((arg==) . modName) modulesL of
		_ | arg == "*"	-> f =<< getModules
		Just x		-> f [x]
		Nothing		-> Error >>> "No module matching \"" ++ arg ++ "\"."
	where
	arg = fmap toLower $ firstWord mess
	f xs = do
		state <- get
		put =<< (io $ execRiver (mapM_ (\x -> modFinish x >> modInit x ) xs) state)
		Echo >>> view "Modules restarted" (intercalate ", " (fmap modName xs))

comCommands _ = do
	xs <- getModules
	Echo >>> (unwords . fmap format) xs
	where format Module{modName, modList} = view modName (intercalate ", " (sort $ fmap fst modList))

comHelp mess
	| null mess = do
		k <- gets (comkey . config)
		Echo >>> "Use " ++ k ++ "commands or " ++ k ++ "aliases for a list of available functions."

	| otherwise = do
		commands	<- getsCom commands
		let arg		= map toLower $ firstWord mess
		case M.lookup arg commands of
			Nothing -> do
				query <- ComAlias.fetchAlias arg
				Echo >>> case query of
					Nothing		-> view arg "Command or alias not found."
					Just (a, b)	-> "(alias) " ++ arg ++ " \STX->\STX " ++ a ++ " " ++ b

			Just (_,_,_,help, info)	-> Echo >>> "\STX" ++ arg ++ helpargs ++ ":\STX " ++ info
				where helpargs = (if not $ null help then " " else "") ++ help

getModules :: (MonadState (RState x) m) => m [Module]
getModules = do
	exclude <- filter (/="core") `liftM` gets (modulesexcl . config)
	return $ filter (flip notElem exclude . modName) modulesL
