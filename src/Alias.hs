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
	unless ("aliases" `elem` tables) $ do
		run conn create []
		commit conn
	where
	create = "CREATE TABLE aliases ("
		++ "id       SERIAL PRIMARY KEY,"
		++ "alias    TEXT NOT NULL UNIQUE,"
		++ "value    TEXT NOT NULL"
		++ ")"

fetchAlias :: (IConnection c) => c -> String -> IO (Maybe String)
fetchAlias conn key = do
	query <- quickQuery' conn "SELECT value FROM aliases WHERE alias = ? LIMIT 1" [toSql (fmap toLower key)]
	return $ case query of
		[[a]]	-> Just $ fromSql a
		_	-> Nothing

allAlias :: (IConnection c) => c -> IO [String]
allAlias conn = fmap f `fmap` quickQuery' conn "SELECT alias FROM aliases" []
	where f = \[a] -> fromSql a

addAlias :: (IConnection c) => c -> String -> String -> IO Bool
addAlias conn key value
	| null key || null value	= return False
	| otherwise			= falsefail conn $ do
		run conn "INSERT INTO aliases (alias, value) VALUES (?, ?)"
			[toSql $ fmap toLower key, toSql value]
		commit conn

delAlias :: (IConnection c) => c -> String -> IO Bool
delAlias conn key = falsefail conn $ do
	run conn "DELETE FROM aliases WHERE alias = ?" [toSql (fmap toLower key)]
	commit conn

falsefail :: IConnection c => c -> IO a -> IO Bool
falsefail conn x = handleSql (const $ rollback conn >> return False) (x >> return True)
