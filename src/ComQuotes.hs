module ComQuotes(list, initialize) where
import System.Time
import System.Locale
import qualified Data.Map as M
import Data.List
import Control.Monad
import Database.HDBC

import CommandInterface

list :: CommandList
list = [
	  ("flame"		, (get Flame		, 1	, Peon	, "<victim>"
		, "What can be more insulting than having an ircbot flame you?"))
	, ("flame-add"		, (put Flame		, 1	, Peon	, "<<insult>>"
		, "Add a flame to the database. Use %s for the victim's name and %t for the current time. /me is supported."))
	, ("love"		, (get Love		, 1	, Peon	, "<lucky person>"
		, "Share some love!"))
	, ("love-add"		, (put Love		, 1	, Peon	, "<<love>>"
		, "Add a love to the database. Use %s for the loved's name and %t for the current time. /me is supported."))
	]

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless ("quotes" `elem` tables) $ do
		run conn create []
		run conn "CREATE UNIQUE INDEX quotes_nocase ON quotes (LOWER(quote))" []
		commit conn
	where
	create = "CREATE TABLE quotes (\
		\id       SERIAL PRIMARY KEY,\
		\ident    CHAR(1) NOT NULL,\
		\nick     TEXT NOT NULL,\
		\quote    TEXT NOT NULL\
		\)"

data Quote = Flame | Love
	deriving (Show)

dbIdent :: Quote -> SqlValue
dbIdent = toSql . head . show

nickfix :: Quote -> Info -> String -> String -> ClockTime -> String -> String
nickfix ident Info{myNick, userList} user target time str = case ident of
	Flame	| M.member (Nocase target) userList && Nocase target /= myNick ->
			compile target
		| otherwise ->
			compile user

	Love	| Nocase target == myNick ->
			":D"
		| target =|= user || M.notMember (Nocase target) userList ->
			user ++ ", share love and you shall recieve."
		| otherwise ->
			compile target

	where	compile nick = replace ("%s", nick) . replace ("%t", timeFormat time) $ str
		timeFormat t = formatCalendarTime defaultTimeLocale "%A %H:%M UTC" (toUTCTime t)

get, put :: Quote -> Command
get ident (Name (Nocase nick) _ _) mess info@Info{echo} ComState{conn} = do
	q	<- quickQuery' conn "SELECT quote FROM quotes WHERE ident = ? ORDER BY RANDOM() LIMIT 1" [dbIdent ident]
	time	<- getClockTime
	echo $ nickfix ident info nick (firstWord mess) time (quoteFound q)
	where quoteFound = maybeL ("No "++show ident++"-quotes found, so I'll just ping %s instead :D") (fromSql . head)

put ident nick mess Info{echo} ComState{conn}
	| isInfixOf "%s" mess = let
		err _	= echo "The string is already in the database."
		try _	= do
			run conn "INSERT INTO quotes (ident, nick, quote) VALUES (?, ?, ?)"
				[dbIdent ident, toSql $ show nick, toSql mess]
			echo $ show ident ++ " added: \""++mess++"\""
		in handleSql err $ withTransaction conn try
	| otherwise =
		echo $ "\"%s\" is required in the string."
