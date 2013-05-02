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
		[ ("clanlist"		, (clanList	, 0	, Peon	, ""
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

clanList, clanInfo :: Command State


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

