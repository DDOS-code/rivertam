module Memos (
	Entry
	, initialize
	, saveMemos
	, fetchMemos
) where
import Control.Monad
import Database.HDBC
import Helpers
import System.Time
import System.Locale

data Entry = Entry !Integer !String !String

instance Show Entry where
	show (Entry time from mess) = date ++ " - Message from " ++ from ++ ": " ++ mess
		where date = formatCalendarTime defaultTimeLocale "%c" (toUTCTime $ TOD time 0)

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless ("memos" `elem` tables) $ do
		run conn create []
		commit conn
	where create = "CREATE TABLE memos ("
		++ "unixtime    INTEGER         NOT NULL,"
		++ "receiver    TEXT            NOT NULL,"
		++ "sender      TEXT            NOT NULL,"
		++ "mess        TEXT            NOT NULL"
		++ ")"

saveMemos :: (IConnection c) => c -> String -> String -> String -> IO ()
saveMemos conn key_ nick mess  = do
	time <- getUnixTime
	run conn "INSERT INTO memos VALUES (?, ?, ?, ?)"
		[toSql time, toSql key, toSql nick, toSql mess]
	commit conn
	where key = fmap toLower key_

fetchMemos :: (IConnection c) => c -> String -> IO [Entry]
fetchMemos conn key_ = do
	query	<- quickQuery' conn "SELECT * FROM memos WHERE receiver = ?" [toSql key]
	unless (null query) $ do
		run conn "DELETE FROM memos WHERE receiver = ?" [toSql key]
		commit conn
	return $ map sqlToEntry query
	where	key = fmap toLower key_
		sqlToEntry = \[a, _, c, d] -> Entry (fromSql a) (fromSql c) (fromSql d)
