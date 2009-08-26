module Config(
	Access(..)
	, Config(..)
	, getConfig
) where
import Data.Map(Map)
import Data.List
import Data.Maybe
import Control.Monad (liftM)
import qualified Data.Map as M
import Network

import IRC
import Helpers

data Access = Mute | Peon | User | Master deriving (Eq, Ord, Read, Show)

data MayFail a b = Fail a | Success b
instance Monad (MayFail a) where
	return				= Success
	(Fail e)	>>=	_	= Fail e
	(Success x)	>>=	f	= f x

mayFailToEither :: MayFail a b -> Either a b
mayFailToEither (Fail x)	= Left x
mayFailToEither (Success x)	= Right x

data Config = Config{
	  network	:: String
	, port		:: PortNumber
	, pgconn	:: String
	, debug 	:: Int
	, nick		:: Nocase
	, user
	, name
	, nickserv
	, comkey	:: String
	, channels 	:: [(Nocase, String)]
	, access 	:: [(Access, Name)]
	, queryaccess	:: Access
	, reparsetime	:: Int

	, cacheinterval :: Integer
	, clanlist	:: [String]
	, polldns 	:: (Map String String)
	, tremdedchan
	, tremdedrcon
	, tremdedhost	:: String
	, tremdedfifo	:: FilePath
	} deriving Eq


getConfig :: String -> Either String Config
getConfig = mayFailToEither . getConfig'

getConfig' :: String -> MayFail String Config
getConfig' cont = do
	--Required
	network		<- look "network"
	nick		<- Nocase `liftM` look "nick"
	comkey		<- look "comkey"
	pgconn		<- look "pgconn"

	--Optional
	user		<- lookOpt "user"	"rivertam"
	name		<- lookOpt "name"	"River Tam - Cadynum's pet"
	nickserv	<- lookOpt "nickserv"	""
	channels	<- chanFormat `liftM` lookOpt "channels"	[]
	debug		<- lookOpt "debug"	1
	clanlist	<- nubBy (=|=) `liftM` lookOpt "clanlist"	[]
	port		<- fromInteger `liftM` lookOpt "port" 6667
	access		<- (\xs -> [(a, b) | (a, readNUH -> Right b) <- xs]) `liftM` lookOpt "access" []
	queryaccess	<- lookOpt "queryaccess" User
	reparsetime	<- (*1000000) `liftM` lookOpt "reparsetime" 60

	polldns		<- M.fromList `liftM` lookOpt "polldns" []
	cacheinterval	<- (*1000000) `liftM` lookOpt "cacheinterval" 60
	tremdedchan	<- lookOpt "tremdedchan" ""
	tremdedfifo	<- lookOpt "tremdedfifo" ""
	tremdedrcon	<- lookOpt "tremdedrcon" ""
	tremdedhost	<- lookOpt "tremdedhost" ""

	return $ Config {
		  network
		, port
		, pgconn
		, nick
		, user
		, name
		, nickserv
		, channels
		, debug
		, comkey
		, access
		, queryaccess
		, polldns
		, reparsetime
		, cacheinterval
		, clanlist
		, tremdedchan
		, tremdedfifo
		, tremdedrcon
		, tremdedhost
		}
	where
		tuples		= map lineToTuple (lines cont)
		look key	= case lookup key tuples of
					Nothing	-> Fail $ "Required field not found: " ++ key
					Just a	-> eread key a
		lookOpt key d	= maybe (Success d) (eread key) $ lookup key tuples
		eread key	= maybe (Fail $ "Error parsing: "++key) Success . mread

lineToTuple :: String -> (String, String)
lineToTuple x = (a, stripw b)
	where (a, b) = break isSpace . dropWhile isSpace $ x

chanFormat :: [String] -> [(Nocase, String)]
chanFormat = catMaybes . map (f . words) where
	f [a]		= Just (Nocase a, "")
	f [a, b]	= Just (Nocase a, b)
	f _		= Nothing
