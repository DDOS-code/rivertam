module ComAlias (m, fetchAlias) where
import CommandInterface
import Data.List (intercalate)
import qualified Data.Map as M

m :: Module
comAliases, comAliasDel, comAliasAdd :: Command
fetchAlias :: (MonadState (RState x) m, MonadIO m) => String -> m (Maybe String)

m = Module
	{ modName	= "alias"
	, modInit	= sqlIfNotTable "aliases"
		["CREATE TABLE aliases (\
		\key      TEXT PRIMARY KEY,\
		\value    TEXT NOT NULL\
		\)"]
	, modFinish	= return ()
	, modList	=
		[ ("aliases"		, (comAliases	, 0	, Peon		, ""
			, "Lists all aliases."))
		, ("aliasdel"		, (comAliasDel	, 1	, User		, "<alias>"
			, "Deletes an alias."))
		, ("aliasadd"		, (comAliasAdd	, 2	, User		, "<alias> <<value>>"
			, "Adds an alias. The alias cannot exist."))
		]
	}

comAliases _  = do
	query <- sqlQuery "SELECT key FROM aliases ORDER BY key" []
	Echo >>> "Aliases are: " ++ intercalate ", " (map (fromSql . head) query)

comAliasDel args = do
	let key	= fmap toLower $ firstWord args
	x <- sqlTransaction $ sqlRun "DELETE FROM aliases WHERE key = ?" [toSql key]
	Echo >>> view key (if x > 0 then "Deleted." else "Not found.")

comAliasAdd args
	| any (not . isAlphaNum) key =
		Error >>> "Only alphanumeric chars allowed in aliases."
	| otherwise  = do
		commands <- M.keys <$> getsCom commands
		if first `notElem` commands
			then Error >>> '"' : first ++ "\" is not a valid command."
			else let
				err _	= Error >>> "Failed. Probably because it's already existing."
				try 	= do
					sqlRun "INSERT INTO aliases VALUES (?, ?)"
						[toSql key, toSql value]
					Echo >>> "Alias \"" ++ key ++ "\" added."
				in sqlTransactionTry try err
	where	(key'', value)	= breakDrop isSpace args
		key 		= fmap toLower key''
		first		= fmap toLower $ firstWord value

fetchAlias key = do
	query <- sqlQuery' "SELECT value FROM aliases WHERE key = ?" [toSql (fmap toLower key)]
	return $ maybeL Nothing (Just . fromSql . head) query
