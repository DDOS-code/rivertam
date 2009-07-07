module Memos (
	Entry
	, initialize
	, saveMemos
	, fetchMemos
) where
import Control.Monad
import Database.HDBC
import Data.Char
import System.Time
import System.Locale

data Entry = Entry ClockTime String String

instance Show Entry where
	show (Entry time from mess) = date ++ " - Message from " ++ from ++ ": " ++ mess
		where date = formatCalendarTime defaultTimeLocale "%c" (toUTCTime time)

sqlToEntry :: [SqlValue] -> Entry
sqlToEntry [a, _, c, d] = Entry (TOD (fromSql a) 0) (fromSql c) (fromSql d)
sqlToEntry _ = error "Memos table error"

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless (any (=="memos") tables) $ do
		run conn create []
		commit conn
	where create = "CREATE TABLE memos ("
		++ "unixtime    INTEGER         NOT NULL,"
		++ "receiver    TEXT            NOT NULL,"
		++ "sender      TEXT            NOT NULL,"
		++ "mess        TEXT            NOT NULL"
		++ ")"

saveMemos :: (IConnection c) => c -> String -> String -> String -> IO Bool
saveMemos conn key_ nick mess  = handleSql (const $ return False) $ do
	TOD time _ <- getClockTime
	run conn "INSERT INTO memos VALUES (?, ?, ?, ?)"
		[toSql time, toSql key, toSql nick, toSql mess]
	commit conn
	return True
	where key = fmap toLower key_

fetchMemos :: (IConnection c) => c -> String -> IO [Entry]
fetchMemos conn key_ = do
	query	<- quickQuery' conn "SELECT * FROM memos WHERE receiver = ?" [toSql key]
	when (not $ null query) $ do
		run conn "DELETE FROM memos WHERE receiver = ?" [toSql key]
		commit conn
	return $ fmap sqlToEntry query
	where key = fmap toLower key_
