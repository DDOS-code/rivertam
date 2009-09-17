module ComClans (list, initialize) where
import Control.Monad
import Database.HDBC
import Text.Read

import CommandInterface

list :: CommandList
list = [
	("clan-add"	, (clanAdd	, 4	, User	, "\"tag\" \"name\" \"irc channel\" \"homepage url\""
		, "Add a clan to the database. NOTE: Every argument needs to be quoted."))
	, ("clan-del"	, (clanDel	, 1	, User	, "<clan-tag>"
		, "Removes a clan from the database."))
	, ("clan-list"	, (clanList	, 0	, Peon	, ""
		, "Lists all clans."))
	, ("clan-info"	, (clanInfo	, 1	, Peon	, "<clan-tag>"
		, "Info about a particular clan."))
	]

initialize :: (IConnection c) => c -> IO ()
initialize conn = do
	tables <- getTables conn
	unless ("clans" `elem` tables) $ do
		run conn create []
		run conn "CREATE UNIQUE INDEX clans_nocase ON clans (LOWER(tag))" []
		commit conn
	where
	create = "CREATE TABLE clans (\
		\id          SERIAL PRIMARY KEY,\
		\tag         TEXT NOT NULL,\
		\name        TEXT NOT NULL,\
		\irc         TEXT NOT NULL,\
		\homepage    TEXT NOT NULL\
		\)"


data InsertClan = InsertClan !String !String !String !String
instance Read InsertClan where
	readPrec = let f = (\(String x) -> return x) =<< lexP
			in liftM4 InsertClan f f f f

clanAdd, clanDel, clanList, clanInfo :: Command

clanAdd _ mess Info{echo} ComState{conn} = case mread mess :: Maybe InsertClan of
	Just (InsertClan tag name irc homepage) | all (not . null) [tag, name]	-> let
		err _	= echo ("Adding \""++tag++"\" Failed: Already existing.")
		try _	= do
			run conn "INSERT INTO clans (tag, name, irc, homepage) VALUES (?, ?, ?, ?)"
				[toSql tag, toSql name, toSql irc, toSql homepage]
			echo $ name ++ " added."
		in handleSql err $ withTransaction conn try

	_	-> echo "\STXclan-add:\STX Error in syntax."

clanDel _ clan Info{echo} ComState{conn} = withTransaction conn $ const $ do
	x <- run conn "DELETE FROM clans WHERE LOWER(tag) = LOWER(?)" [toSql clan]
	echo $ if x > 0
		then "Clan \"" ++ clan ++ "\" successfully removed."
		else "Removing \"" ++ clan ++ "\" failed: Tag doesn't exist."

clanList _ _ Info{echo} ComState{conn} = do
	q <- quickQuery conn "SELECT tag FROM clans ORDER BY LOWER(tag)" []
	echo $ "Clans: " ++ (intercalate ", " $ map (fromSql . head) q)

clanInfo _ mess Info{echo} ComState{conn} = do
	q <- quickQuery' conn "SELECT tag, name, irc, homepage FROM clans WHERE tag ILIKE ('%' || ? || '%')" [toSql clan]
	echo $ case q of
		[] -> clan ++ ": Not found."
		[[tag, name, irc, homepage]] ->
			foldr f [] [("Tag", tag), ("Name", name), ("Irc", irc), ("Homepage", homepage)]
		xs -> "Possible choices: " ++ (intercalate ", " $ fmap (fromSql . head) xs)

	where	clan = firstWord mess
		f (view, raw) xs = case fromSql raw of
					""	-> xs
					x	-> '\STX':view ++ ":\STX " ++ x ++ " " ++ xs
