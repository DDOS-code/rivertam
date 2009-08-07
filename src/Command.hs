module Command (
	command
	, initComState
	, finalizeComState
) where
import Data.Map(Map)
import qualified Data.Map as M
import Data.List
import Data.Maybe

import System.Info
import Data.Version
import Data.IORef
import Control.Concurrent.STM
import Control.Monad

import CommandInterface
import GeoIP
import System.Time
import Database.HDBC
import Database.HDBC.PostgreSQL
import Memos
import IRC

import qualified ComAlias
import qualified ComTrem
import qualified ComCW
import qualified ComFlameLove
import qualified ComTimers
import qualified ComMemos
import qualified TremRelay
import TremPolling

cList :: CommandList
cList = sortBy (\(a, _) (b, _) -> compare a b) $ modules

cListMap :: Map String CommandInfo
cListMap = M.fromList cList

modules :: CommandList
modules = essential ++ ComFlameLove.list ++ ComTrem.list ++ ComTimers.list ++ ComMemos.list ++ ComCW.list
	++ TremRelay.list ++ ComAlias.list

initComState :: FilePath -> Config -> (IRC.Response -> IO ()) -> IO ComState
initComState configpath config x  = do
	conn		<- connectPostgreSQL (pgconn config)
	TOD uptime _	<- getClockTime
	geoIP		<- fromFile $ configpath ++ "IpToCountry.csv"
	poll		<- newIORef emptyPoll
	pollTime	<- newIORef 0
	pollHost	<- newIORef =<< getDNS "master.tremulous.net" "30710"
	counter		<- newIORef 0
	countdownS	<- atomically $ newTVar M.empty
	relay		<- newIORef =<< TremRelay.initialize config x

	Memos.initialize conn
	ComAlias.initialize conn
	ComFlameLove.initialize conn
	ComCW.initialize conn

	return $! ComState {
		  conn
		, uptime
		, geoIP
		, pollTime
		, poll
		, pollHost
		, counter
		, countdownS
		, relay
		}

finalizeComState :: ComState -> IO ()
finalizeComState ComState{conn} = do
	disconnect conn

command :: Info -> ComState -> Access -> String -> String -> IO ()
command info state@ComState{conn} accesslevel nick mess
	| (not $ null fname) && accesslevel >= Peon =
		case M.lookup fname cListMap of
			Nothing	-> do
				test <- ComAlias.fetchAlias conn fname
				case test of
					Nothing	-> echop $ "\STX" ++ fname ++ ":\STX Command or alias not found."
					Just a	-> command info state accesslevel nick (a ++ ' ':fargs)

			Just (f ,args, access, help, _)
				| accesslevel < access ->
					echo $ "\STX"++fname++":\STX "++show access++"-access or higher needed."
				| not $ (atLeastLen args $ words fargs) ->
					echo $ "Missing arguments, usage: "++fname++" "++help
				| otherwise	-> do
					start	<- getMicroTime
					f nick fargs info state
					end	<- getMicroTime
					when (debug config >= 1) $
						putStrLn $ "Command " ++ fname ++ " time: " ++ show ((end-start) // 1000) ++ "ms"

	| otherwise = return ()
	where
	(a0, aE)	= break isSpace mess
	fname		= map toLower a0
	fargs		= stripw aE
	Info {echo, echop, config} = info


-- // Essential Commands //
essential :: CommandList
essential =
	[("help"		, (comHelp	, 1	, Peon		, "<command/alias>"
		, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
	, ("about"		, (comAbout	, 0	, Peon		, ""
		, "Brief info about the bot."))
	, ("echo"		, (comEcho	, 1	, Peon		, "<<string>>"
		, "Echoes back whatever argumet you supply. \"%s\" will get replaced with your nick. Good for creating aliases."))
	, ("pingall"		, (comPingall	, 0	, User		, ""
		, "Will echo back a list of every user in the channel."))
	, ("commands"		, (comCommands	, 0	, Peon		, ""
		, "Lists all commands."))
	, ("source"		, (comSource	, 0	, Peon		, ""
		, "Displays git url."))
	]


comEcho, comAbout, comSource, comCommands, comHelp, comPingall :: Command

comEcho nick mess Info{echo} _ = echo $ replace ("%s", nick) mess

comAbout _ _ Info{echo}_  = echo $
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ _ Info{echo} _ = echo $ "git clone git://git.mercenariesguild.net/rivertam.git"

comCommands _ _  Info{echo, config} _ = echo $ "Commands are (key: "++comkey config++"): " ++ commands
	where commands = intercalate ", " . map fst $ cList

comHelp _ mess Info{echo} ComState{conn} =
	case M.lookup arg cListMap of
		Nothing -> do
			query <- ComAlias.fetchAlias conn arg
			echo $ case query of
				Nothing	-> "\STX" ++ arg ++ ":\STX Command or alias not found."
				Just a	-> "(alias) " ++ arg ++ " \STX->\STX " ++ a
		Just (_,_,_,help,info)	-> echo $ "\STX" ++ arg ++ helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
	where arg = head $ words mess

comPingall _ _ Info {userList, echo} _ = do
	case M.keys userList of
		[]	-> echo $ "\STXpingall:\STX No users found."
		a	-> mapM_ echo $ neatList $ fmap decase a

	-- Max length for an irc message is 512 chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x
