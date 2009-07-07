module Config(
	Access(..)
	, Config(..)
	, getConfig
) where
import Data.Map(Map)
import Data.List
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
	, debug 	:: Int
	, nick
	, user
	, name
	, nickserv
	, comkey	:: String
	, channels 	:: [(String, String)]
	, access 	:: [(Access, Sender)]
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
	nick		<- look "nick"
	comkey		<- look "comkey"


	--Optional
	user		<- lookOpt "user"	"rivertam"
	name		<- lookOpt "name"	"River Tam - Cadynum's pet"
	nickserv	<- lookOpt "nickserv"	""
	channels	<- chanFormat `liftM` lookOpt "channels"	[]
	debug		<- lookOpt "debug"	1
	clanlist	<- nubBy (=|=) `liftM` lookOpt "clanlist"	[]
	port		<- fromInteger `liftM` lookOpt "port" 6667
	access		<- (map (\(a, b) -> (a, either (const $ Server "fa!l@d") id (readNUH b)))) `liftM` lookOpt "access" []
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

chanFormat :: [String] -> [(String, String)]
chanFormat = map (f . words) where
	f (a:b:_)	= (a, b)
	f (a:_)		= (a, "")
	f _		= ("", "")
