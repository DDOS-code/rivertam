module Config(
	  module Data.Map
	, Access(..)
	, Config(..)
	, getConfig
) where
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad
import Network

import Helpers

data Access = Peon | User | Master deriving (Eq, Ord, Read, Show)


data Config = Config
	{ network	:: String
	, port		:: PortNumber
	, debug 	:: Int
	, nick
	, user
	, name
	, nickserv
	, comkey	:: String
	, channels 	:: [(String, String)]
	, access 	:: [(Access, (String, String, String))]
	, alias 	:: (Map String String)

	, cacheinterval :: Integer
	, clanlist	:: [String]
	, polldns 	:: (Map String String)
	, tremdedchan	:: String
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
	clanlist	<- lookOpt "clanlist"	[]
	port		<- fromInteger `liftM` lookOpt "port" 6667
	access		<- map (\(a, b) -> (a, nicksplit b)) `liftM` lookOpt "access" []
	polldns		<- M.fromList `liftM` lookOpt "polldns" []
	cacheinterval	<- (*1000000) `liftM` lookOpt "cacheinterval" 60
	tremdedchan	<- lookOpt "tremdedchan" ""
	tremdedfifo	<- lookOpt "tremdedfifo" ""
	alias		<- M.fromList `liftM` lookOpt "alias" []

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
		, polldns
		, alias
		, cacheinterval
		, clanlist
		, tremdedchan
		, tremdedfifo
		}
	where
		tuples		= map lineToTuple (lines cont)
		look key	= case lookup key tuples of
					Nothing	-> Left ("Required field not found: " ++ key)
					Just a	-> eread key a
		lookOpt key d	= maybe (Right d) (\a -> eread key a) $ lookup key tuples
		eread key a = maybe (Left $ "Error parsing: "++key) Right (mread a)

lineToTuple :: String -> (String, String)
lineToTuple x = (a, b)
	where	cx	= stripw x
		(a, b')	= break isSpace cx
		b	= dropWhile isSpace b'


instance Monad (Either a) where
	return				= Right
	(Left e)	>>=	_	= Left e
	(Right x)	>>=	f	= f x

