module ComMemos (m, fetchMemos) where
import CommandInterface
import System.Time
import System.Locale

m :: Module
m = Module
	{ modName	= "memos"
	, modInit	= sqlIfNotTable "memos" ["CREATE TABLE memos (\
		\unixtime    INTEGER NOT NULL,\
		\receiver    TEXT NOT NULL,\
		\sender      TEXT NOT NULL,\
		\mess        TEXT NOT NULL\
		\)"]
	, modFinish	= return ()
	, modList	=
		[ ("memo"		, (comMemo	, 2	, Peon	, "<receiver> <<message>>"
		, "Store a memo which will be sent once any indication of life is found from the receiver."))
		]
	}

data Entry = Entry !Integer !String !String

instance Show Entry where
	show (Entry time from mess) = date ++ " - Message from " ++ from ++ ": " ++ mess
		where date = formatCalendarTime defaultTimeLocale "%c" (toUTCTime $ TOD time 0)

comMemo :: Command
comMemo args = do
	Info{nickName=Nocase nick, domain} <- ask
	time		<- io getUnixTime
	let from	= nick ++ '!':domain
	sqlTransaction $ sqlRun "INSERT INTO memos VALUES (?, ?, ?, ?)"
		[toSql time, toSql $ fmap toLower reciever, toSql from, toSql mess]
	Echo >>> nick ++ ", Memo to " ++ reciever ++ " saved."
	where (reciever, mess) = breakDrop isSpace args

fetchMemos :: Nocase -> River [Entry]
fetchMemos (Nocase keyU) = do
	query <- sqlQuery' "SELECT * FROM memos WHERE receiver = ?" [toSql key]
	unless (null query) $ do
		sqlRun "DELETE FROM memos WHERE receiver = ?" [toSql key]
		sqlArg0 commit
	return $ map sqlToEntry query
	where	key = fmap toLower keyU
		sqlToEntry = \[a, _, c, d] -> Entry (fromSql a) (fromSql c) (fromSql d)
