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
import Control.Monad

import CommandInterface
import GeoIP
import Database.HDBC
import Database.HDBC.PostgreSQL
import Memos
import IRC

import qualified ComAlias
import qualified ComTrem
import qualified ComCW
import qualified ComClans
import qualified ComQuotes
import qualified ComTimers
import qualified ComMemos
import qualified TremRelay
import TremPolling

commandMap :: Map String CommandInfo
commandMap = M.fromList modules

modules :: CommandList
modules = essential ++ ComQuotes.list ++ ComTrem.list ++ ComTimers.list ++ ComMemos.list
	++ ComCW.list ++ TremRelay.list ++ ComAlias.list ++ ComClans.list

initComState :: FilePath -> Config -> (IRC.Response -> IO ()) -> IO ComState
initComState configpath config x  = do
	conn		<- connectPostgreSQL (pgconn config)
	uptime		<- getUnixTime
	geoIP		<- fromFile $ configpath ++ "IpToCountry.csv"
	poll		<- newIORef emptyPoll
	pollTime	<- newIORef 0
	pollHost	<- newIORef =<< getDNS "master.tremulous.net" "30710"
	relay		<- newIORef =<< TremRelay.initialize config x

	Memos.initialize conn
	ComAlias.initialize conn
	ComQuotes.initialize conn
	ComCW.initialize conn
	ComClans.initialize conn

	return $! ComState {
		  conn
		, uptime
		, geoIP
		, pollTime
		, poll
		, pollHost
		, relay
		}

finalizeComState :: ComState -> IO ()
finalizeComState ComState{conn, relay} = do
	TremRelay.finalize =<< readIORef relay
	disconnect conn

command :: Info -> ComState -> Access -> Name -> String -> IO ()
command info state@ComState{conn} accesslevel nick mess
	| (not $ null fname) && accesslevel >= Peon =
		case M.lookup fname commandMap of
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
					let sqlFail e = rollback conn >> print e >> (echo $ "\STX" ++ fname ++ ":\STX SqlError.")
					handleSql sqlFail (f nick fargs info state)
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
	[("help"		, (comHelp	, 0	, Peon		, "<command/alias>"
		, "(arg) = optional argument | <arg> = required argument | ((string)) = optional non-whitespace demited string | <<string>> = required non-whitespace demited string"))
	, ("about"		, (comAbout	, 0	, Peon		, ""
		, "Brief info about the bot."))
	, ("echo"		, (comEcho	, 1	, Peon		, "<<message>>"
		, "Echoes back whatever argumet you supply. \"%s\" will get replaced with your nick. Good for creating aliases. /me is supported."))
	, ("pingall"		, (comPingall	, 0	, User		, ""
		, "Will echo back a list of every user in the channel."))
	, ("commands"		, (comCommands	, 0	, Peon		, ""
		, "Lists all commands."))
	, ("source"		, (comSource	, 0	, Peon		, ""
		, "Displays git url."))
	, ("alias-add"		, (comAliasAdd	, 2	, User		, "<alias> <<value>>"
		, "Adds an alias. The alias cannot exist."))
	]


comEcho, comAbout, comSource, comCommands, comHelp, comPingall :: Command

comEcho (Name (Nocase nick) _ _) mess Info{echo} _ = echo $ replace ("%s", nick) mess

comAbout _ _ Info{echo}_  = echo $
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ _ Info{echo} _ = echo $ "git clone git://git.mercenariesguild.net/rivertam.git"

comCommands _ _ Info{echo} _ = echo $ "Commands are: " ++ commands
	where commands = intercalate ", " $ M.keys commandMap

comHelp _ mess Info{echo, config} ComState{conn}
	| null mess = let k = comkey config in echo $ "Use "++k++"commands or "++k++"aliases for a list of available functions."
	| otherwise = case M.lookup arg commandMap of
		Nothing -> do
			query <- ComAlias.fetchAlias conn arg
			echo $ case query of
				Nothing	-> "\STX" ++ arg ++ ":\STX Command or alias not found."
				Just a	-> "(alias) " ++ arg ++ " \STX->\STX " ++ a
		Just (_,_,_,help,info)	-> echo $ "\STX" ++ arg ++ helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
	where arg = firstWord mess

comPingall _ _ Info {userList, echo} _ = do
	case M.keys userList of
		[]	-> echo $ "\STXpingall:\STX No users found."
		a	-> mapM_ echo $ neatList $ fmap recase a

	-- Max length for an irc message is 512 chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x

--TODO: Split to ComAlias when dynamic modules are introduced.
comAliasAdd :: Command
comAliasAdd _ args Info{echo} ComState{conn}
	| any (not . isAlphaNum) key =
		echo $ "\STXaliasadd:\STX Only alphanumeric chars allowed in aliases."
	| M.notMember first commandMap =
		echo $ "\STXaddalias:\STX \"" ++ first ++ "\" is not a valid command."
	| otherwise  = let
		err _	= rollback conn >> echo "\STXalias-add:\STX Failed. Probably because it's already existing."
		try 	= do
			run conn "INSERT INTO aliases (alias, value) VALUES (?, ?)"
				[toSql key, toSql value]
			commit conn
			echo $ "Alias \"" ++ key ++ "\" added."
		in handleSql err try
	where	(key'', value)	= breakDrop isSpace args
		key 		= fmap toLower key''
		first		= fmap toLower $ firstWord value
