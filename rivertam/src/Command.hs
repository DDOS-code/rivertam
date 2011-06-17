module Command (command) where
import qualified Data.Map as M
import Control.Exception
import Control.Monad
import Irc.Protocol
import Module
import Data.List

command :: Access -> Nocase -> Nocase -> String -> String -> River x ()
command access channel nick domain mess = when (not $ null fname || access < Peon) $ do
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
		--, Handler (\(e :: SqlError)		-> ex e)
		]
	case cond of
		Left e	-> trace e >> (send $ Msg channel $ view fname "Exception raised!")
		Right s	-> put s
	end	<- io getMicroTime
	decho $ "Command " ++ fname ++ " time: " ++ show ((end-start) // 1000) ++ "ms"
	where
	(a0, fargs)	= breakDrop isSpace mess
	fname		= map toLower a0
	ex :: (Monad m, Show s) => s -> m (Either String a)
	ex = return . Left . show

tryCommand :: String -> RiverCom x ()
tryCommand mess = do
	commands	<- gets commands
	name		<- asks commandName
	case M.lookup name commands of
		Nothing	-> do
			c <- gets (aliasHook . hooks) >>= \x -> x name
			case c of
				Nothing	-> Whisper >>> view name "Command or alias not found."
				Just (name2, mess2) ->
					case M.lookup name2 commands of
						Nothing -> Error >>> "Alias Error."
						Just _ | isInfixOf "%a" mess2 && null mess ->
							Error >>> "The alias requires an argument."
						Just a -> do
							nickName <- recase <$> asks nickName
							doCommand a (replace "%s" nickName (replace "%a" mess mess2))

		Just a -> doCommand a mess

doCommand :: CommandInfo x -> String -> RiverCom x ()
doCommand (f, args, access, help, _) fargs = do
	userAccess <- asks userAccess
	case True of
		_ | userAccess < access ->
			Error >>> show access ++ "-access or higher needed."
		_ | not (atLeastLen args $ words fargs) ->
			Error >>> "Missing arguments. Usage: " ++ help
		_ -> f fargs
