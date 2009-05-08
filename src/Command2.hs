module Command2 where
import Data.Map(Map)
import qualified Data.Map as M

import System.Info
import Data.Version


import Config
import Helpers
import CommandInterface
import GeoIP

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
modules = essential ++ ComCW.list ++ ComTrem.list ++ ComFlameLove.list ++ ComTimers.list


initComState :: FilePath -> IO ComState
initComState confdir = do
	uptime		<- getMicroTime
	geoIP		<- fromFile $ confdir ++ "IpToCountry.csv"
	pollHost	<- getDNS "master.tremulous.net" "30710"
	countdownS	<- atomically $ newTVar M.empty
	return $! ComState {
		  echoFunc	= undefined
		, filePath	= confdir
		, conf		= undefined
		, myNick	= ""
		, userList	= []

		, uptime
		, geoIP
		, poll		= PollNone
		, pollHost

		, counter	= 0
		, countdownS
		}

command :: ComState -> Access -> String -> String -> IO ComState
command state accesslevel nick mess = flip execStateT state $ do
	when ((not $ null fname) && accesslevel >= Peon) $
		case M.lookup fname cListMap of
			Nothing	->
				echo . Private $ "\STX"++fname++":\STX Command not found."
			Just (_,_,access,_,_) | accesslevel < access ->
				echo . Mess $ "\STX"++fname++":\STX "++show access++"-access or higher needed."
			Just (_,args,_,help,_) | (length $ words fargs) < args ->
				echo . Mess $ "Missing arguments, usage: "++fname++" "++help
			Just (func, _,_,_,_) ->
				func nick fargs

	where	(a0, aE)	= break isSpace mess
		fname		= map toLower a0
		fargs		= stripw aE


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

comMoo nick mess = echo . Mess $ "Moo Moo, " ++ nick ++ ": " ++ mess

comAbout _ _ = echo . Mess $
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ _ = echo . Mess $ "git clone git://git.mercenariesguild.net/rivertam.git"

comHelp _ mess
	| null mess	= do
		Config {comkey} <- gets conf
		echo . Mess $ "Commands are (key: "++comkey++"): " ++ (intercalate ", " . map fst $ cList)
	| otherwise	= case M.lookup arg cListMap of
		Just (_,_,_,help,info)	-> echo . Mess $ "\STX" ++ arg ++  helpargs ++ ":\STX " ++ info
			where helpargs = (if not $ null help then " " else "") ++ help
		Nothing			-> echo . Private $ "Sorry, I don't know the command \""++arg++"\""
	where arg = head $ words mess

comAlias _ args = do
	Config {alias}	<- gets conf
	echo . Mess $ if null args
		then "Aliases are: " ++ intercalate ", "  [x | (x, _) <- M.toList alias]
		else arg++" \STX->\STX " ++ fromMaybe "No such alias." (M.lookup arg alias)
	where arg = head . words $ args

comPingall _ _ = do
	users		<- gets userList
	case users of
		[]	-> echo . Mess $ "\STXpingall:\STX No users found."
		a	-> mapM_ (echo . Mess) $ neatList a

	-- Max length on an irc message is 512chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x



