module ComEssential (mdl) where
import CommandInterface
import System.Info
import Data.Version
import qualified Data.Map as M
import Data.List

mdl :: Module
mdl = Module
	{ modName	= "utils"
	, modInit	= return ()
	, modFinish	= return ()
	, modList	=
		[ ("about"		, (comAbout	, 0	, Peon		, ""
			, "Brief info about the bot."))
		, ("echo"		, (comEcho	, 1	, Peon		, "<<message>>"
			, "Echoes back whatever argumet you supply. \"%s\" will get replaced with your nick. Good for creating aliases. /me is supported."))
		, ("pingall"		, (comPingall	, 0	, User		, ""
			, "Will echo back a list of every user in the channel."))
		, ("source"		, (comSource	, 0	, Peon		, ""
			, "Displays git url."))
		, ("uptime"		, (comUptime	, 0	, Peon		, ""
			, "Displays uptime (obviously)."))
		]
	}

comEcho, comAbout, comSource, comPingall, comUptime :: Command

comEcho mess = do
	Nocase nick <- asks nickName
	Echo >>> replace "%s" nick mess

comAbout _ = Echo >>>
	"\STXriver-tam\STX, written by Christoffer Öjeling \"Cadynum\" in haskell. Running on "
	++ (capitalize os) ++ " " ++ arch ++ ". Compiler: " ++ compilerName ++ " " ++ showVersion compilerVersion ++ "."

comSource _ = Echo >>> "git clone git://git.mercenariesguild.net/rivertam.git"

comPingall _ = do
	users <- getUserList
	case M.keys users of
		[]	-> Error >>> "No users found."
		a	-> EchoM >>> neatList $ fmap recase a

	-- Max length for an irc message is 512 chars
	-- Max nick-size is 15 + 1 whitespace = 16
	-- 512/16 = 32
	where	neatList []	= []
		neatList x	= unwords a : neatList b where
			(a, b)	= splitAt 32 x

comUptime _ = (Echo >>>) =<< format <$> gets initTime <*> io getUnixTime
	where
	format started now = "Running for " ++ formatTime sec ++ ". e-shoes: " ++ eshoes
		where
		sec	= fromInteger (now-started) ::Int
		day	= sec // 86400 + 1 -- She must have at least 1 pair of shoes, thus the +1 :)
		eshoes	= "|" ++ (replicate day '.') ++ "|"
