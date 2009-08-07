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
	, ("aliasadd"		, (comAliasAdd	, 2	, User		, "<alias> <<value>>"
		, "Adds an alias. The alias cannot exist."))
	, ("aliasdel"		, (comAliasDel	, 1	, User		, "<alias>"
		, "Deletes an alias."))
	]

comAliases, comAliasAdd, comAliasDel :: Command

comAliases _ _ Info{echo, config} ComState{conn} = do
	query 	<- fmap f `fmap` quickQuery' conn "SELECT alias FROM aliases" []
	echo $ "Aliases are (key: "++comkey config++"): " ++ intercalate ", " query
	where f = \[a] -> fromSql a


comAliasAdd _ args Info{echo} ComState{conn}
	| any (not . isAlphaNum) key =
		echo $ "\STXaliasadd:\STX Only alphanumeric chars allowed in aliases."
	-- | M.notMember first cListMap =
	--	echo $ "\STXaddalias:\STX \"" ++ first ++ "\" is not a valid command."
	| otherwise  = let
		err _	= echo $ "\STXaliasadd:\STX Failed. Probably because it's already existing."
		try 	= do
			run conn "INSERT INTO aliases (alias, value) VALUES (?, ?)"
				[toSql $ fmap toLower key, toSql value]
			commit conn
			echo $ "Alias \"" ++ key ++ "\" added."
		in handleSql err try
	where (key, value) = breakDrop isSpace args
	--	first = fmap toLower $ firstWord value

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
