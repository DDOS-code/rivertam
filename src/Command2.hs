module Command2 where
import Data.Map(Map)
import qualified Data.Map as M

import System.Info
import Data.Version


import Config
import Helpers
import CommandInterface
import GeoIP
import System.Time

import Control.Concurrent.STM

import qualified ComCW
import qualified ComTrem
import qualified ComFlameLove
import qualified ComTimers

cList :: CommandList
cList = sortBy (\(a, _) (b, _) -> compare a b) $ modules

cListMap :: Map String CommandInfo
cListMap = M.fromList cList

modules :: CommandList
modules = essential ++ ComCW.list ++ ComTimers.list  ++ ComFlameLove.list ++ ComTrem.list


initComState :: FilePath -> IO ComState
initComState confdir = do
	TOD uptime _	<- getClockTime
	geoIP		<- fromFile $ confdir ++ "IpToCountry.csv"
	pollHost	<- getDNS "master.tremulous.net" "30710"
	countdownS	<- atomically $ newTVar M.empty
	return $! ComState {
		  uptime
		, geoIP
		, poll		= PollNone
		, pollHost

		, counter	= 0
		, countdownS
		}

command :: Info -> ComState -> Access -> String -> String -> IO ComState
command info state accesslevel nick mess = do
	if (not $ null fname) && accesslevel >= Peon then do
		case M.lookup fname cListMap of
			Nothing	-> do
				echo2 . Private $  "\STX"++fname++":\STX Command not found."
				return state
			Just (_,_,access,_,_) | accesslevel < access -> do
				echo2 . Mess $ "\STX"++fname++":\STX "++show access++"-access or higher needed."
				return state
			Just (_,args,_,help,_) | (length $ words fargs) < args -> do
				echo2 . Mess $ "Missing arguments, usage: "++fname++" "++help
				return state
			Just (func, _,_,_,_) ->
				execStateT (func nick fargs info) state
		else return state

	where	(a0, aE)	= break isSpace mess
		fname		= map toLower a0
		fargs		= stripw aE
		echo2		= echoFunc2 info


-- // Essential Commands //
essential :: CommandList
essential =
	[("help"		, (comHelp	, 0	, Peon		, "(command)"
		, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
	, ("about"		, (comAbout	, 0	, Peon		, ""
		, "Brief info about the bot."))
	, ("moo"		, (comMoo	, 0	, Peon		, "((string))"
		, "Test function, will echo back the string."))
	, ("pingall"		, (comPingall	, 0	, User		, ""
		, "Will echo back a list of every user in the channel."))
	, ("alias"		, (comAlias	, 0	, Peon		, "(alias-key)"
		, "List of the current aliases, or with an argument expand the alias."))
	, ("source"		, (comSource	, 0	, Peon		, ""
		, "Displays git url."))
	]


comMoo, comAbout, comSource, comHelp, comAlias, comPingall :: Command

comMoo nick mess Info {echoFunc2} = lift . echoFunc2 . Mess $ "Moo Moo, " ++ nick ++ ": " ++ mess

comAbout _ _ Info {echoFunc2} = lift . echoFunc2 . Mess $
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ _ _ = echo . Mess $ "git clone git://git.mercenariesguild.net/rivertam.git"

comHelp _ mess Info {config2}
	| null mess	= do
		echo . Mess $ "Commands are (key: "++comkey config2++"): " ++ (intercalate ", " . map fst $ cList)
	| otherwise	= case M.lookup arg cListMap of
		Just (_,_,_,help,info)	-> echo . Mess $ "\STX" ++ arg ++  helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
		Nothing			-> echo . Private $ "Sorry, I don't know the command \""++arg++"\""
	where arg = head $ words mess

comAlias _ args  Info { config2 = Config {alias} }= do
	echo . Mess $ if null args
		then "Aliases are: " ++ intercalate ", "  [x | (x, _) <- M.toList alias]
		else arg++" \STX->\STX " ++ fromMaybe "No such alias." (M.lookup arg alias)
	where arg = head . words $ args


comPingall _ _ Info {userList} = do
	case userList of
		[]	-> echo . Mess $ "\STXpingall:\STX No users found."
		a	-> mapM_ (echo . Mess) $ neatList a

	-- Max length on an irc message is 512chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x
