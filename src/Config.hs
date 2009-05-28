module Config(
	  module Data.Map
	, Access(..)
	, Config(..)
	, getConfig
) where
import Data.Map(Map)
import qualified Data.Map as M
import Data.Either
import Control.Monad
import Network

import IRC
import Helpers

data Access = Mute | Peon | User | Master deriving (Eq, Ord, Read, Show)

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
	, alias 	:: (Map String String)
	, reparsetime	:: Integer

	, cacheinterval :: Integer
	, clanlist	:: [String]
	, polldns 	:: (Map String String)
	, tremdedchan
	, tremdedrcon
	, tremdedhost	:: String
	, tremdedfifo	:: FilePath
	} deriving Eq


getConfig :: String -> Either String Config
getConfig cont = do
	--Required
	network		<- look "network"
	nick		<- look "nick"
	comkey		<- look "comkey"

	--Optional
	user		<- lookOpt "user"	"rivertam"
	name		<- lookOpt "name"	"River Tam - Cadynum's pet"
	nickserv	<- lookOpt "nickserv"	""
	channels	<- lookOpt "channels"	[]
	debug		<- lookOpt "debug"	1
	clanlist	<- nubBy (=|=) `liftM` lookOpt "clanlist"	[]
	port		<- fromInteger `liftM` lookOpt "port" 6667
	access		<- (map (\(a, b) -> (a, either (const $ Server "fa!l@d") id (readNUH b)))) `liftM` lookOpt "access" []
	queryaccess	<- lookOpt "queryaccess" User
	alias		<- M.fromList `liftM` lookOpt "alias" []
	reparsetime	<- lookOpt "reparsetime" 60

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
		, alias
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
					Nothing	-> Left ("Required field not found: " ++ key)
					Just a	-> eread key a
		lookOpt key d	= maybe (Right d) (eread key) $ lookup key tuples
		eread key	= maybe (Left $ "Error parsing: "++key) Right . mread

lineToTuple :: String -> (String, String)
lineToTuple x = (a, stripw b)
	where (a, b) = break isSpace . dropWhile isSpace $ x

instance Monad (Either a) where
	return				= Right
	(Left e)	>>=	_	= Left e
	(Right x)	>>=	f	= f x
