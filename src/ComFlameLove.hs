module ComFlameLove(list) where
import System.Random
import Text.Printf
import System.Time
import System.IO.Error
import Prelude hiding (catch)
import qualified Data.ByteString.Char8 as B

import CommandInterface
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

getRandom :: (FilePath, FilePath) -> String -> IO String
getRandom (f1, f2) person = do
	content		<- randomFromList . filter (not . B.null) =<< (B.lines `liftM` B.readFile (file++".conf")) `catch` (\_-> return [])
	time		<- getClockTime >>= toCalendarTime
	let daytime	= printf "%s %02d:%02d" (show (ctWDay time)) (ctHour time) (ctMin time)

	return . replace (("%s", person)) . replace ("%t", daytime) $ content
	where
		file 			= f1++f2
		randomFromList []	= return $ "%s, I would love to "++f2++" you, but i can't find the database."
		randomFromList lst	= (B.unpack . (lst!!)) `liftM` (getStdRandom . randomR $ (0, length lst-1))

comFlame, comLove :: Command

comFlame snick mess = do
	confDir		<- getFP ""
	myNick		<- gets myNick
	userList	<- gets userList
	let	arg	= head . words $ mess
		nickl	= map toLower arg
		test	= nickl `elem` userList
		victim	= if test && not (arg =|= myNick) then arg else snick

	line		<- lift $ getRandom (confDir,"flame") victim

	echo . Mess $ line


comLove snick mess = do
	confDir		<- getFP ""
	myNick		<- gets myNick
	userList	<- gets userList

	let	arg	= head . words $ mess
		nickl	= map toLower arg
		test	= nickl `elem` userList

	line 		<- lift $ getRandom (confDir,"love") arg

	let a	| arg =|= myNick		= ":D"
		| test && not (arg =|= snick)	= line
		| otherwise			= snick++", share love and you shall recieve."
		in echo . Mess $ a

comXadd :: (String, String) -> Command
comXadd (text, file) _ args
	| isInfixOf "%s" args = do
		fp <- getFP file
		lift $ appendFile fp (args++"\n")
		echo . Mess $ text++" added, \""++args++"\""
	| otherwise		=
		echo . Mess $ "\"%s\" is required in the string."
