module Memos (
	Memos
	, Entry
	, initMemos
	, saveMemos
	, fetchMemos
) where
import Control.Monad
import Database.HDBC
import Data.Char
import System.Time
import System.Locale


data (IConnection c) => Memos c = Memos {
	    conn	:: !c
	  , fetch	:: !String
	  , insert
	  , delete	:: !Statement
	  }

data Entry = Entry ClockTime String String

instance Show Entry where
	show (Entry time from mess) = date ++ " - Message from " ++ from ++ ": " ++ mess
		where date = formatCalendarTime defaultTimeLocale "%c" (toUTCTime time)

create :: String
create = "CREATE TABLE memos ("
	++ "unixtime    INTEGER         NOT NULL,"
	++ "receiver    TEXT            NOT NULL,"
	++ "sender      TEXT            NOT NULL,"
	++ "mess        TEXT            NOT NULL"
	++ ")"

sqlToEntry :: [SqlValue] -> Entry
sqlToEntry [a, _, c, d] = Entry (TOD (fromSql a) 0) (fromSql c) (fromSql d)
sqlToEntry _ = error "Memos table error"

initMemos :: (IConnection c) => c -> IO (Memos c)
initMemos conn = do
	tables <- getTables conn
	unless (any (=="memos") tables) $ do
		run conn create []
		commit conn
		return ()
	liftM2 (Memos conn "SELECT * FROM memos WHERE receiver = ?")
		(f "INSERT INTO memos VALUES (?, ?, ?, ?)")
		(f "DELETE FROM memos WHERE receiver = ?")
	where f = prepare conn

saveMemos :: (IConnection c) => Memos c -> String -> String -> String -> IO ()
saveMemos Memos{conn, insert} key_ nick mess  = do
	TOD time _ <- getClockTime
	execute insert [toSql time, toSql key, toSql nick, toSql mess]
	commit conn
	where key = fmap toLower key_

fetchMemos :: (IConnection c) => Memos c -> String -> IO [Entry]
fetchMemos Memos{conn, fetch, delete} key_ = do
	query	<- quickQuery' conn fetch [toSql key]
	execute delete [toSql key]
	commit conn
	return $ fmap sqlToEntry query
	where key = fmap toLower key_

