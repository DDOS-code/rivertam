module Module.Clans (mdl, withClan, withClanPlayed) where
import Module
import Module.State
import Module.RiverHDBC
import Text.Read
import Data.List (intercalate)

mdl :: Module State
mdl = Module
	{ modName	= "clan"
	, modInit	= sqlIfNotTable "clans" ["CREATE TABLE clans (\
		\id          SERIAL PRIMARY KEY,\
		\tag         TEXT NOT NULL,\
		\name        TEXT NOT NULL,\
		\irc         TEXT DEFAULT '',\
		\homepage    TEXT DEFAULT ''\
		\)"
		, "CREATE UNIQUE INDEX clans_nocase ON clans (LOWER(tag))"]
	, modFinish	= return ()
	, modList	=
		[ ("clanadd"		, (clanAdd	, 4	, Peon	, "\"tag\" \"name\" \"irc channel\" \"homepage url\""
			, "Add a clan to the database. NOTE: Every argument needs to be quoted."))
		, ("clanupdate"		, (clanUpdate	, 4	, User	, "\"tag\" \"name\" \"irc channel\" \"homepage url\""
			, "Update an existing clan."))
		, ("clandel"		, (clanDel	, 1	, User	, "<clan-tag>"
			, "Removes a clan from the database."))
		, ("clanlist"		, (clanList	, 0	, Peon	, ""
			, "Lists all clans."))
		, ("claninfo"		, (clanInfo	, 1	, Peon	, "<clan-tag>"
			, "Info about a particular clan."))
		]
	}

data InsertClan = InsertClan !String !String !String !String
instance Read InsertClan where
	readPrec = do
		String a <- lexP
		String b <- lexP
		String c <- lexP
		String d <- lexP
		return $ InsertClan (stripw a) (stripw b) (stripw c) (stripw d)

clanAdd, clanUpdate, clanDel, clanList, clanInfo :: Command State

clanAdd mess = case mread mess of
	Just (InsertClan tag name irc homepage) | all (not . null) [tag, name]	-> let
		err _	= Echo >>> '"' : tag ++ "\" Already existing."
		try	= do
			sqlRun "INSERT INTO clans (tag, name, irc, homepage) VALUES (?, ?, ?, ?)"
				[toSql tag, toSql name, toSql irc, toSql homepage]
			Echo >>> name ++ " added."
		in sqlTransactionTry try err

	_	-> Error >>> "Error in syntax."

clanUpdate mess = case mread mess of
	Just (InsertClan tag name irc homepage) | all (not . null) [tag, name]	-> sqlTransaction $ do
		x <- sqlRun "UPDATE clans SET tag = ?, name = ?, irc = ?, homepage = ? WHERE LOWER(tag) = LOWER(?)"
			[toSql tag, toSql name, toSql irc, toSql homepage, toSql tag]
		Echo >>> name ++ ": " ++ (if x > 0 then "modified." else "not found.")

	_	-> Error >>> "Error in syntax."

clanDel clan = let
	err _	= Echo >>> "Can't remove \"" ++ clan ++ "\", clangames depends on it."
	try	= do
		x <- sqlRun "DELETE FROM clans WHERE LOWER(tag) = LOWER(?)" [toSql clan]
		Echo >>> if x > 0
			then "Clan \"" ++ clan ++ "\" successfully removed."
			else "Removing \"" ++ clan ++ "\" failed: Tag doesn't exist."
	in sqlTransactionTry try err

clanList _ = do
	q <- sqlQuery "SELECT tag FROM clans ORDER BY LOWER(tag)" []
	Echo >>> "Clans: " ++ (intercalate ", " . map (fromSql . head)) q

clanInfo mess = withClan (firstWord mess) $ \xs ->
	let [_, tag, name, irc, homepage] = map fromSql xs
	in Echo >>> foldr f [] [("Tag", tag), ("Name", name), ("Irc", irc), ("Homepage", homepage)]
	where
	f (_, "") xs	= xs
	f (txt, raw) xs	= view txt raw ++ ' ':xs

withClan, withClanPlayed :: String -> ([SqlValue] -> RiverCom State ()) -> RiverCom State ()
withClan	= withClanGeneric "SELECT id, tag, name, irc, homepage FROM clans WHERE tag ILIKE ('%' || ? || '%')"
withClanPlayed	= withClanGeneric "SELECT DISTINCT ON (clan) clans.id, tag, name FROM clans JOIN cw_games ON clan = clans.id WHERE tag ILIKE ('%' || ? || '%')"

withClanGeneric :: String -> String -> ([SqlValue] -> RiverCom State ()) -> RiverCom State()
withClanGeneric query clan f = do
	q <- sqlQuery query [toSql clan]
	case q of
		[]	-> Echo >>> clan ++ ": Not found."
		[x]	-> f x
		xs	-> Echo >>> "Possible choices: " ++ (intercalate ", " . map (\(_:tag:_) -> fromSql tag)) xs

