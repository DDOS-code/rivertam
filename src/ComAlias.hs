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
		++ "alias    TEXT PRIMARY KEY,"
		++ "value    TEXT NOT NULL"
		++ ")"

list :: CommandList
list = [  ("aliases"		, (comAliases	, 0	, Peon		, ""
		, "Lists all aliases."))
	, ("alias-del"		, (comAliasDel	, 1	, User		, "<alias>"
		, "Deletes an alias."))
	]

comAliases, comAliasDel :: Command

comAliases _ _ Info{echo} ComState{conn} = do
	query <- quickQuery' conn "SELECT alias FROM aliases ORDER BY alias" []
	echo $ "Aliases are: " ++ intercalate ", " (map (fromSql . head) query)

comAliasDel _ args Info{echo} ComState{conn} = do
	let key	= fmap toLower $ firstWord args
	x <- run conn "DELETE FROM aliases WHERE alias = ?" [toSql key]
	commit conn
	echo $ "\STX" ++ key ++ "\STX: " ++ (if x > 0 then "Deleted" else "Not found") ++ "."

fetchAlias :: (IConnection c) => c -> String -> IO (Maybe String)
fetchAlias conn key = do
	query <- quickQuery' conn "SELECT value FROM aliases WHERE alias = ?" [toSql (fmap toLower key)]
	return $ case query of
		[[a]]	-> Just $ fromSql a
		_	-> Nothing
