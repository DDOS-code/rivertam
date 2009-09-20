module Command (
	command, commandInit, commandFinish
) where
import Data.Map(Map)
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

modules :: [Module]
modules = [ComEssential.wee, ComAlias.m, ComQuotes.m, ComMemos.m, ComClans.m, ComCW.m, ComTrem.m, ComTremRelay.info]

commandMap :: Map String CommandInfo
commandMap = M.fromList $ concatMap modList modules

commandInit, commandFinish :: River ()
commandInit = mapM_ modInit modules
commandFinish = mapM_ modFinish modules

command :: Nocase -> Access -> Name -> String -> River ()
command channel accesslevel nuh@(Name nick _ _) mess
	| (not $ null fname) && accesslevel >= Peon =
		case M.lookup fname commandMap of
			Nothing	-> do
				c <- ComAlias.fetchAlias fname
				case c of
					Nothing	-> send $ Notice nick $ view fname "Command or alias not found."
					Just a	-> command channel accesslevel nuh (a ++ ' ':fargs)
			Just (f, args, access, help, _)
				| accesslevel < access ->
					send $ Msg channel $ view fname (show access++"-access or higher needed.")
				| not $ (atLeastLen args $ words fargs) ->
					send $ Msg channel $ "Missing arguments, usage: "++fname++" "++help
				| otherwise	-> do
					start	<- io getMicroTime
					let info = Info {userAccess=accesslevel, channel, commandName = fname, userName=nuh, modulesI=modules}
					state 	<- get
					cond	<- io $ catches ((Right . snd) <$> runRiverCom (f fargs) info state)
						[ Handler (\(e :: ArithException)	-> ex e)
						, Handler (\(e :: PatternMatchFail)	-> ex e)
						, Handler (\(e :: ErrorCall)		-> ex e)
						, Handler (\(e :: IOException)		-> ex e)
						, Handler (\(e :: ArithException)	-> ex e)
						, Handler (\(e :: SqlError)		-> rollback (conn state) >> ex e)
						]
					case cond of
						Left e	-> trace e >> (send $ Msg channel $ view fname "Exception raised!")
						Right s	-> put s
					end	<- io getMicroTime
					echo $ "Command " ++ fname ++ " time: " ++ show ((end-start) // 1000) ++ "ms"

	| otherwise = return ()
	where
	(a0, fargs)	= breakDrop isSpace mess
	fname		= map toLower a0
	ex :: (Monad m, Show s) => s -> m (Either String a)
	ex = return . Left . show
