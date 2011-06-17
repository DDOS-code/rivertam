module Config(
	Access(..), Config(..), getConfig
) where
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.DeepSeq
import Network

import Irc.Protocol
import Helpers

data Access = Mute | Peon | User | Master deriving (Eq, Ord, Read, Show)
instance NFData Access

newtype Mayfail a b = Mayfail {toEither :: Either a b}
instance Monad (Mayfail a) where
	return					= Mayfail . Right
	Mayfail (Left e)	>>=	_	= Mayfail $ Left e
	Mayfail (Right x)	>>=	f	= f x

data Config = Config
	{ network	:: !String
	, port		:: !PortNumber
	, pgconn	:: !String
	, debug 	:: !Int
	, nick		:: !Nocase
	, user
	, password
	, name
	, comkey	:: !String
	, channels 	:: ![(Nocase, String)]
	, access 	:: ![(Access, Name)]
	, queryaccess	:: !Access
	, reparsetime	:: !Int
	, modulesexcl	:: ![String]

	, cacheinterval :: !Integer
	, masterserver	:: ![(String, String, Int)]
	, tremdedchan	:: !Nocase
	, tremdedrcon
	, tremdedhost	:: !String
	, tremdedfifo	:: !FilePath
	}

getConfig :: String -> Either String Config
getConfig = toEither . evalStateT getConfig' . mapMaybe lineToTuple . splitlines


getConfig' :: StateT [(String, String)] (Mayfail String) Config
getConfig' = do
	--Required
	network		<- require "network"
	nick		<- require "nick"
	comkey		<- require "comkey"
	pgconn		<- require "pgconn"

	--Optional
	user		<- optional	"user"		"rivertam"
	name		<- optional	"name"		"River Tam - Cadynum's pet"
	password	<- optional	"password"	""
	channels	<- optionalWith	"channels"	[] 		chanFormat
	debug		<- optional	"debug"		1
	port		<- optionalWith	"port"		6667 		fromInteger
	access		<- optionalWith	"access"	[] 		accessFormat
	queryaccess	<- optional	"queryaccess"	User
	reparsetime	<- optionalWith	"reparsetime"	60 		(*1000000)
	modulesexcl	<- optional	"modulesexcl"	[]

	cacheinterval	<- optionalWith	"cacheinterval"	60 		(*1000000)
	masterserver	<- optional	"masterserver"	[("vanilla", "master.tremulous.net:30710", 69)]
	tremdedchan	<- optional	"tremdedchan"	(Nocase "")
	tremdedfifo	<- optional	"tremdedfifo"	""
	tremdedrcon	<- optional	"tremdedrcon"	""
	tremdedhost	<- optional	"tremdedhost"	""

	return Config {..}
	where
	look fnothing fjust key = get >>= \s -> case lookupDelete key s of
				(Nothing, _)	-> lift fnothing
				(Just a, s')	-> put s' >> lift (fjust a)

	require key		= look (Mayfail . Left $ "Required field not found: " ++ key) (eread id key) key
	optionalWith key def f	= look (Mayfail . Right . f $ def) (eread f key) key
	optional k def 		= optionalWith k def id
	eread f key 		= maybe (Mayfail $ Left $ "Error parsing: "++key) (Mayfail . Right . f . strict) . mread

lineToTuple :: String -> Maybe (String, String)
lineToTuple x = let	(a, b)	= breakDrop isSpace $ stripw x
			a'	= map toLower a
		in if valid a' b then Just (a', b) else Nothing
	where
	valid key value = (not $ null key) && (not $ null value) && all isAlpha key

chanFormat :: [String] -> [(Nocase, String)]
chanFormat = mapMaybe (f . words) where
	f [a]		= Just (Nocase a, "")
	f [a, b]	= Just (Nocase a, b)
	f _		= Nothing

accessFormat :: [(Access, String)] -> [(Access, Name)]
accessFormat xs = [(a, b) | (a, readNUH -> Right b) <- xs]
