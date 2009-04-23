module ComFlameLove(list) where
import System.Random
import qualified Data.Map as M
import Text.Printf
import System.Time
import System.IO.Error
import Prelude hiding (catch)

import Send
import RiverState
import Config
import Helpers

list :: CommandList
list =
	[ ("flame"		, (comFlame				, 1	, Peon	, "<victim>"
		, "What can be more insulting than having an ircbot flaming you?"))
	, ("flameadd"		, (comXadd ("Flame", "flame.conf")	, 1	, Peon	, "<<insult>>"
		, "Add a flame to the database. Use %s for the victim's name and %t for the current time."))
	, ("love"		, (comLove				, 1	, Peon	, "<lucky person>"
		, "Share some love!"))
	, ("loveadd"		, (comXadd ("Love", "love.conf")	, 1	, Peon	, "<<love>>"
		, "Add a love to the database. Use %s for the loved's name and %t for the current time."))
	]

getRandom :: FilePath -> String -> IO String
getRandom file person = do
	content <- readFile (file++".conf") `catch` (\_-> return []) >>= return . splitlines >>= randomFromList
	time <- getClockTime >>= toCalendarTime
	let daytime = printf "%s %02d:%02d" (show (ctWDay time)) (ctHour time) (ctMin time)
	return . replace (("%s", person)) . replace ("%t", daytime) $ content
	where
		randomFromList [] = return $ "%s, I would love to "++file++" you, but i can't find the database."
		randomFromList lst = (lst!!) `liftM` (getStdRandom . randomR $ (0, length lst-1))

comFlame, comLove :: Command

comFlame (snick, chan, mess) = do
	rivConfDir	<- gets rivConfDir
	Config {nick}	<- gets config
	rivMap		<- gets rivMap
	let	arg	= head . words $ mess
		chanl	= map toLower chan
		nickl	= map toLower arg
		test	= isJust $ M.lookup chanl rivMap >>= M.lookup nickl
		victim = if test && not (arg =|= nick) then arg else snick

	line		<- lift $ getRandom (rivConfDir++"flame") victim
	Msg chan >>> line


comLove (snick, chan, mess) = do
	rivConfDir	<- gets rivConfDir
	Config {nick}	<- gets config
	rivMap		<- gets rivMap

	let	arg	= head . words $ mess
		chanl	= map toLower chan
		nickl	= map toLower arg
		test	= isJust $ M.lookup chanl rivMap >>= M.lookup nickl

	line 		<- lift $ getRandom (rivConfDir++"love") arg

	let a	| arg =|= nick			= ":D"
		| test && not (arg =|= snick)	= line
		| otherwise			= snick++", share love and you shall recieve."
		in Msg chan >>> a

comXadd :: (String, String) -> Command
comXadd (text, file) (_, chan, args)
	| isInfixOf "%s" args	= do
		rivConfDir <- gets rivConfDir
		lift $ appendFile (rivConfDir++file) (args++"\n")
		Msg chan >>> text++" added, \""++args++"\""
	| otherwise		=
		Msg chan >>> "\"%s\" is required in the string."
