module ComClans (m) where
import CommandInterface
import Text.Read
import Data.List (intercalate)

m :: Module
m = Module
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
		[ ("clan-add"		, (clanAdd	, 4	, Peon	, "\"tag\" \"name\" \"irc channel\" \"homepage url\""
			, "Add a clan to the database. NOTE: Every argument needs to be quoted."))
		, ("clan-update"	, (clanUpdate	, 4	, User	, "\"tag\" \"name\" \"irc channel\" \"homepage url\""
			, "Update an existing clan."))
		, ("clan-del"		, (clanDel	, 1	, User	, "<clan-tag>"
			, "Removes a clan from the database."))
		, ("clan-list"		, (clanList	, 0	, Peon	, ""
			, "Lists all clans."))
		, ("clan-info"		, (clanInfo	, 1	, Peon	, "<clan-tag>"
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

clanAdd, clanUpdate, clanDel, clanList, clanInfo :: Command

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
	Echo >>> "Clans: " ++ (intercalate ", " $ map (fromSql . head) q)

clanInfo mess = do
	q <- sqlQuery "SELECT tag, name, irc, homepage FROM clans WHERE tag ILIKE ('%' || ? || '%')" [toSql clan]
	Echo >>> case fmap (fmap fromSql) q of
		[] -> clan ++ ": Not found."
		[[tag, name, irc, homepage]] ->
			foldr f [] [("Tag", tag), ("Name", name), ("Irc", irc), ("Homepage", homepage)]
		xs -> "Possible choices: " ++ (intercalate ", " $ fmap head xs)

	where	clan = firstWord mess
		f (_, "") xs	= xs
		f (txt, raw) xs	= view txt raw ++ ' ':xs
