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

{-import qualified ComCW
import qualified ComTrem
import qualified ComFlameLove
import qualified ComTimers-}
import TremMasterCache

cList :: CommandList
cList = sortBy (\(a, _) (b, _) -> compare a b) $ modules

cListMap :: Map String CommandInfo
cListMap = M.fromList cList

modules :: CommandList
modules = essential -- ++ ComCW.list ++ ComTimers.list  ++ ComFlameLove.list ++ ComTrem.list


initComState :: FilePath -> FilePath -> IO ComState
initComState _ datapath = do
	TOD uptime _	<- getClockTime
	geoIP		<- fromFile $ datapath ++ "IpToCountry.csv"
	pollHost	<- getDNS "master.tremulous.net" "30710"
	countdownS	<- atomically $ newTVar M.empty
	return $! ComState {
		  uptime
		, geoIP
		, pollTime	= 0
		, poll		= emptyPoll
		, pollHost

		, counter	= 0
		, countdownS
		}

command :: Info -> ComState -> Access -> String -> String -> IO ComState
command info state accesslevel nick mess = do
	if (not $ null fname) && accesslevel >= Peon then do
		case M.lookup fname cListMap of
			Nothing	-> do
				echop $ "\STX"++fname++":\STX Command not found."
				return state
			Just (_,_,access,_,_) | accesslevel < access -> do
				echo $ "\STX"++fname++":\STX "++show access++"-access or higher needed."
				return state
			Just (_,args,_,help,_) | (length $ words fargs) < args -> do
				echo $ "Missing arguments, usage: "++fname++" "++help
				return state
			Just (func, _,_,_,_) ->
				execStateT (func nick fargs info) state
		else return state

	where	(a0, aE)	= break isSpace mess
		fname		= map toLower a0
		fargs		= stripw aE
		Info {echo, echop} = info

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

comMoo nick mess Info{echo} = lift . echo $ "Moo Moo, " ++ nick ++ ": " ++ mess

comAbout _ _ Info{echo} = lift . echo $
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ _ Info{echo} = lift . echo $ "git clone git://git.mercenariesguild.net/rivertam.git"

comHelp _ mess Info{config2, echo, echop}
	| null mess	= do
		lift $ echo $ "Commands are (key: "++comkey config2++"): " ++ (intercalate ", " . map fst $ cList)
	| otherwise	= case M.lookup arg cListMap of
		Just (_,_,_,help,info)	-> lift $ echo $ "\STX" ++ arg ++  helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
		Nothing			-> lift $ echop $ "Sorry, I don't know the command \""++arg++"\""
	where arg = head $ words mess

comAlias _ args  Info { config2 = Config{alias, comkey}, echo }= do
	lift $ echo $ if null args
		then "Aliases are (key: "++comkey++comkey++"): " ++ intercalate ", "  (M.keys alias)
		else arg++" \STX->\STX " ++ fromMaybe "No such alias." (M.lookup arg alias)
	where arg = head . words $ args


comPingall _ _ Info {userList, echo} = do
	case userList of
		[]	-> lift $ echo $ "\STXpingall:\STX No users found."
		a	-> mapM_ (lift . echo) $ neatList a

	-- Max length on an irc message is 512chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x
