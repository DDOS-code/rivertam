module Command where
import Data.Map(Map)
import qualified Data.Map as M

import System.Info
import Data.Version
import Data.IORef
import Control.Concurrent.STM
import Control.Monad

import Config
import Helpers
import CommandInterface
import GeoIP
import System.Time
import Database.HDBC.Sqlite3
import Memos

import qualified ComTrem
import qualified ComCW
import qualified ComFlameLove
import qualified ComTimers
import qualified ComMemos
import TremMasterCache

cList :: CommandList
cList = sortBy (\(a, _) (b, _) -> compare a b) $ modules

cListMap :: Map String CommandInfo
cListMap = M.fromList cList

modules :: CommandList
modules = essential ++ ComFlameLove.list ++ ComCW.list ++ ComTrem.list ++ ComTimers.list ++ ComMemos.list

initComState :: FilePath -> FilePath -> IO ComState
initComState configpath datapath = do
	conn		<- connectSqlite3 (configpath++"river.db")
	TOD uptime _	<- getClockTime
	geoIP		<- fromFile $ datapath ++ "IpToCountry.csv"
	poll		<- newIORef emptyPoll
	pollTime	<- newIORef 0
	pollHost	<- newIORef =<< getDNS "master.tremulous.net" "30710"
	counter		<- newIORef 0
	countdownS	<- atomically $ newTVar M.empty
	memos		<- initMemos conn

	ComFlameLove.initialize conn

	return $! ComState {
		  conn
		, uptime
		, geoIP
		, pollTime
		, poll
		, pollHost
		, counter
		, countdownS
		, memos
		}

command :: Info -> ComState -> Access -> String -> String -> IO ()
command info state accesslevel nick mess
	| (not $ null fname) && accesslevel >= Peon =
		case M.lookup fname cListMap of
			Nothing	-> case M.lookup fname alias of
				Nothing	-> return ()
				Just a	-> command info state accesslevel nick (a ++ ' ':fargs)

			Just (f ,args, access, help, _)
				| accesslevel < access ->
					echo $ "\STX"++fname++":\STX "++show access++"-access or higher needed."
				| (length $ words fargs) < args ->
					echo $ "Missing arguments, usage: "++fname++" "++help
				| otherwise	-> f nick fargs info state

	| otherwise = return ()

	where
	(a0, aE)	= break isSpace mess
	fname		= map toLower a0
	fargs		= stripw aE
	Info {echo, config=Config{alias}} = info


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

comMoo nick mess Info{echo} _ = echo $ "Moo Moo, " ++ nick ++ ": " ++ mess

comAbout _ _ Info{echo}_  = echo $
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ _ Info{echo} _ = echo $ "git clone git://git.mercenariesguild.net/rivertam.git"

comHelp _ mess Info{config, echo, echop} _
	| null mess	= do
		echo $ "Commands are (key: "++comkey config++"): " ++ (intercalate ", " . map fst $ cList)
	| otherwise	= case M.lookup arg cListMap of
		Just (_,_,_,help,info)	-> echo $ "\STX" ++ arg ++  helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
		Nothing			-> echop $ "Sorry, I don't know the command \""++arg++"\""
	where arg = head $ words mess

comAlias _ args Info{config = Config{alias, comkey}, echo }  _= do
	echo $ if null args
		then "Aliases are (key: "++comkey++"): " ++ intercalate ", "  (M.keys alias)
		else arg++" \STX->\STX " ++ fromMaybe "No such alias." (M.lookup arg alias)
	where arg = head . words $ args


comPingall _ _ Info {userList, echo} _ = do
	case M.keys userList of
		[]	-> echo $ "\STXpingall:\STX No users found."
		a	-> mapM_ echo $ neatList a

	-- Max length for an irc message is 512 chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x
