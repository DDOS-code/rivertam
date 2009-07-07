module Alias (
	initialize
	, fetchAlias
	, allAlias
	, addAlias
	, delAlias
) where
import Control.Monad
import Database.HDBC
import Data.Char

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless (any (=="aliases") tables) $ do
		run conn create []
		commit conn
	where
	create = "CREATE TABLE aliases ("
		++ "id       INTEGER PRIMARY KEY AUTOINCREMENT,"
		++ "alias    TEXT NOT NULL UNIQUE COLLATE NOCASE,"
		++ "value    TEXT NOT NULL"
		++ ")"

fetchAlias :: (IConnection c) => c -> String -> IO (Maybe String)
fetchAlias conn key = do
	query <- quickQuery' conn "SELECT value FROM aliases WHERE alias = ? LIMIT 1" [toSql key]
	return $ case query of
		[[a]]	-> Just $ fromSql a
		_	-> Nothing

allAlias :: (IConnection c) => c -> IO [String]
allAlias conn = fmap f `fmap` quickQuery' conn "SELECT alias FROM aliases" []
	where f = \[a] -> fromSql a

addAlias :: (IConnection c) => c -> String -> String -> IO Bool
addAlias conn key value
	| null key || null value	= return False
	| otherwise			= falsefail $ do
		run conn "INSERT INTO aliases (alias, value) VALUES (?, ?)"
			[toSql $ fmap toLower key, toSql value]
		commit conn

delAlias :: (IConnection c) => c -> String -> IO Bool
delAlias conn key = falsefail $ do
	run conn "DELETE FROM aliases WHERE alias = ?" [toSql key]
	commit conn

falsefail :: IO a -> IO Bool
falsefail x = handleSql (const $ return False) (x >> return True)
