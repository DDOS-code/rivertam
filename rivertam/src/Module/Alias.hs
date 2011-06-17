module Module.Alias (mdl, alias) where
import Module
import Module.State
import Module.RiverHDBC
import Control.Monad.Trans
import Data.List (intercalate)
import qualified Data.Map as M

mdl :: Module State
mdl = Module
	{ modName	= "alias"
	, modInit	= sqlIfNotTable "aliases"
		["CREATE TABLE aliases (\
		\key      TEXT PRIMARY KEY,\
		\value    TEXT NOT NULL\
		\)"]
	, modFinish	= return ()
	, modList	=
		[ ("aliases"		, (aliases	, 0	, Peon		, ""
			, "Lists all aliases."))
		, ("alias"		, (aliasAdd	, 2	, User		, "<alias> <<value>>"
			, "Adds an alias. %s will get replaced with the sender, and %a with the additional arguments."))
		, ("unalias"		, (aliasDel	, 1	, User		, "<alias>"
			, "Deletes an alias."))

		]
	}

aliases, aliasDel, aliasAdd :: Command State
aliases _  = do
	query <- sqlQuery "SELECT key FROM aliases ORDER BY key" []
	Echo >>> "Aliases are: " ++ intercalate ", " (map (fromSql . head) query)

aliasDel args = do
	let key	= fmap toLower $ firstWord args
	x <- sqlTransaction $ sqlRun "DELETE FROM aliases WHERE key = ?" [toSql key]
	Echo >>> view key (if x > 0 then "Deleted." else "Not found.")

aliasAdd args
	| any (not . isAlphaNum) key =
		Error >>> "Only alphanumeric chars allowed in aliases."
	| otherwise  = do
		commands <- M.keys <$> gets commands
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

alias :: (MonadState (RState State) m, MonadIO m) => String -> m (Maybe (String, String))
alias key = do
	query <- sqlQuery' "SELECT value FROM aliases WHERE key = ?" [toSql (fmap toLower key)]
	return $ maybeL Nothing (Just . sep . fromSql . head) query
	where sep x = let (a, b) = (breakDrop isSpace . stripw) x in (map toLower a, b)
