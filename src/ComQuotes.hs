module ComQuotes (m) where
import CommandInterface
import System.Time
import System.Locale
import qualified Data.Map as M
import Data.List

m :: Module
m = Module
	{ modName	= "quotes"
	, modInit	= sqlIfNotTable "quotes"
		["CREATE TABLE quotes (\
		\id       SERIAL PRIMARY KEY,\
		\ident    CHAR(1) NOT NULL,\
		\nick     TEXT NOT NULL,\
		\quote    TEXT NOT NULL\
		\)"
		, "CREATE UNIQUE INDEX quotes_nocase ON quotes (LOWER(quote))"]
	, modFinish	= return ()
	, modList	=
		[ ("flame"		, (getQ Flame		, 1	, Peon	, "<victim>"
			, "What can be more insulting than having an ircbot flame you?"))
		, ("flameadd"		, (putQ Flame		, 1	, Peon	, "<<insult>>"
			, "Add a flame to the database. Use %s for the victim's name and %t for the current time. /me is supported."))
		, ("love"		, (getQ Love		, 1	, Peon	, "<lucky person>"
			, "Share some love!"))
		, ("loveadd"		, (putQ Love		, 1	, Peon	, "<<love>>"
			, "Add a love to the database. Use %s for the loved's name and %t for the current time. /me is supported."))
		]
	}

data Quote = Flame | Love
	deriving (Show)

dbIdent :: Quote -> SqlValue
dbIdent = toSql . head . show

nickfix :: Quote -> Nocase -> Nocase -> String -> ClockTime -> Nocase -> M.Map Nocase a -> String
nickfix ident user target str time myNick userList = case ident of
	Flame	| M.member target userList && target /= myNick ->
			compile target
		| otherwise ->
			compile user

	Love	| target == myNick ->
			":D"
		| target == user || M.notMember target userList ->
			recase user ++ ", share love and you shall recieve."
		| otherwise ->
			compile target

	where	compile (Nocase nick) = replace "%s" nick . replace "%t" (timeFormat time) $ str
		timeFormat t = formatCalendarTime defaultTimeLocale "%A %H:%M UTC" (toUTCTime t)

getQ, putQ :: Quote -> Command
getQ ident target = do
	q	<- sqlQuery' "SELECT quote FROM quotes WHERE ident = ? ORDER BY RANDOM() LIMIT 1" [dbIdent ident]
	nick	<- asks nickName
	(Echo >>>) =<< nickfix ident nick (Nocase $ firstWord target) (quoteFound q) <$> io getClockTime <*> getRiverNick <*> getUserList
	where quoteFound = maybeL ("No "++show ident++"-quotes found, so I'll just ping %s instead :D") (fromSql . head)

putQ ident mess
	| isInfixOf "%s" mess = let
		err _	= Error >>> "The string is already in the database."
		try	= do
			Info{nickName=Nocase nick, domain} <- ask
			let from	= nick ++ '!':domain
			sqlRun "INSERT INTO quotes (ident, nick, quote) VALUES (?, ?, ?)"
				[dbIdent ident, toSql from, toSql mess]
			Echo >>> show ident ++ " added: \""++mess++"\""
		in sqlTransactionTry try err
	| otherwise =
		Error >>> "\"%s\" is required in the string."
