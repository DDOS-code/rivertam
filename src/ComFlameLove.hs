module ComFlameLove(list, initialize) where
import System.Time
import System.Locale
import qualified Data.Map as M
import Data.List
import Control.Monad
import Database.HDBC


import CommandInterface
import Config
import Helpers

list :: CommandList
list = [
	  ("flame"		, (get Flame		, 1	, Peon	, "<victim>"
		, "What can be more insulting than having an ircbot flame you?"))
	, ("flameadd"		, (put Flame		, 1	, Peon	, "<<insult>>"
		, "Add a flame to the database. Use %s for the victim's name and %t for the current time."))
	, ("love"		, (get Love		, 1	, Peon	, "<lucky person>"
		, "Share some love!"))
	, ("loveadd"		, (put Love		, 1	, Peon	, "<<love>>"
		, "Add a love to the database. Use %s for the loved's name and %t for the current time."))
	]

data Quote = Flame | Love
	deriving (Show)

dbIdent :: Quote -> SqlValue
dbIdent x = toSql $ case x of
	Flame	-> 'F'
	Love	-> 'L'

nickfix :: Quote -> Info -> String -> String -> ClockTime -> String -> String
nickfix ident Info{myNick, userList} user target time str = case ident of
	Flame	| Caseless target == myNick ->
			"Go off and headbutt a bullet, " ++ user ++ "."
		| M.member (Caseless target) userList ->
			compileString target time str
		| otherwise ->
			compileString user time str

	Love	| Caseless target == myNick ->
			":D"
		| target =|= user || M.notMember (Caseless target) userList ->
			user ++ ", share love and you shall recieve."
		| otherwise ->
			compileString target time str

getQuote :: (IConnection c) => c -> Quote -> IO String
getQuote conn ident = do
	x	<- quickQuery' conn "SELECT quote FROM quotes WHERE ident = ? ORDER BY RANDOM() LIMIT 1" [dbIdent ident]
	case x of
		[]	-> return "No quotes found, So I'll just ping %s instead :D"
		[[a]]	-> return $ fromSql a
		_	-> error "Quotes: Bad quote table."

compileString :: String -> ClockTime -> String -> String
compileString nick time = replace ("%s", nick) . replace ("%t", date) where
	date = formatCalendarTime defaultTimeLocale "%A %H:%M UTC" (toUTCTime time)


get, put :: Quote -> Command

get ident nick mess info@Info{echo} ComState{conn} = handleSql err $ do
	time	<- getClockTime
	q	<- getQuote conn ident
	echo $ nickfix ident info nick arg time q
	where
	arg	= takeWhile (not . isSpace) mess
	err _	= echo $ "Database unavailable. (This shouldn't happen)"

put ident nick mess Info{echo} ComState{conn}
	| isInfixOf "%s" mess = let
		sqlerr _= rollback conn >> echo "The string is already in the database."
		try	= do
			run conn "INSERT INTO quotes (ident, nick, quote) VALUES (?, ?, ?)"
				[dbIdent ident, toSql nick, toSql mess]
			commit conn
			echo $ show ident ++ " added, \""++mess++"\""
		in handleSql sqlerr try

	| otherwise		=
		echo $ "\"%s\" is required in the string."

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless ("quotes" `elem` tables) $ do
		run conn create []
		commit conn
	where
	create = "CREATE TABLE quotes ("
		++ "id       SERIAL PRIMARY KEY,"
		++ "ident    CHAR(1) NOT NULL,"
		++ "nick     TEXT NOT NULL,"
		++ "quote    TEXT NOT NULL UNIQUE"
		++ ")"
