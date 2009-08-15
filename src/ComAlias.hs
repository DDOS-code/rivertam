module ComAlias (
	initialize
	, fetchAlias
	, list
) where
import CommandInterface
import Data.List (intercalate)
--import qualified Data.Map as M
import Control.Monad
import Database.HDBC

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

list :: CommandList
list = [  ("aliases"		, (comAliases	, 0	, Peon		, ""
		, "Lists all aliases."))
	, ("aliasdel"		, (comAliasDel	, 1	, User		, "<alias>"
		, "Deletes an alias."))
	]

comAliases, comAliasDel :: Command

comAliases _ _ Info{echo, config} ComState{conn} = do
	query 	<- fmap f `fmap` quickQuery' conn "SELECT alias FROM aliases ORDER BY alias" []
	echo $ "Aliases are (key: "++comkey config++"): " ++ intercalate ", " query
	where f = \[a] -> fromSql a


comAliasDel _ args Info{echo} ComState{conn} = handleSql err try where
	key	= firstWord args
	err _	= echo "Failed to delete alias."
	try	= do
		run conn "DELETE FROM aliases WHERE alias = ?" [toSql (fmap toLower key)]
		commit conn
		echo $ "Alias \"" ++ key ++ "\" deleted."

fetchAlias :: (IConnection c) => c -> String -> IO (Maybe String)
fetchAlias conn key = do
	query <- quickQuery' conn "SELECT value FROM aliases WHERE alias = ?" [toSql (fmap toLower key)]
	return $ case query of
		[[a]]	-> Just $ fromSql a
		_	-> Nothing
