module ComFlameLove(list) where
import System.Random
import Text.Printf
import System.Time
import System.IO.Error
import Prelude hiding (catch)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Monad

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

comFlame snick mess Info {filePath, myNick, userList, echo} _ = do
	echo =<< getRandom (filePath,"flame") victim
	where
	arg	= head . words $ mess
	nickl	= map toLower arg
	test	= M.member nickl userList
	victim	= if test && not (arg =|= myNick) then arg else snick

comLove snick mess Info {filePath, myNick, userList, echo} _ = do
	echo . loveline =<< getRandom (filePath, "love") arg
	where
	loveline x
		| arg =|= myNick		= ":D"
		| test && not (arg =|= snick)	= x
		| otherwise			= snick++", share love and you shall recieve."
	arg	= head . words $ mess
	nickl	= map toLower arg
	test	= M.member nickl userList

comXadd :: (String, String) -> Command
comXadd (text, file) _ args Info{filePath, echo} _
	| isInfixOf "%s" args = do
		appendFile (filePath++file) (args++"\n")
		echo $ text++" added, \""++args++"\""
	| otherwise		=
		echo $ "\"%s\" is required in the string."
