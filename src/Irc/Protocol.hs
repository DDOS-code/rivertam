-- The parsec part of this module is inspired by Network.IRC, written by
-- Trevor Elliott, http://hackage.haskell.org/cgi-bin/hackage-scripts/package/irc
module Irc.Protocol (
	  Status(..), Name(..), Sender(..), Message(..), IRC(..)
	, ircToMessage, responseToIrc, readNUH, p353toTuples
) where
import Text.ParserCombinators.Parsec
import Helpers
import Control.Monad

data Status 	= Normal | Voice | OP deriving (Show, Eq, Ord)
data Name 	= Name !Nocase !Nocase !Nocase
data Sender 	= NUH !Name | Server !Nocase | NoSender
data Message 	= Message !Sender !IRC

type Channel = Nocase

data IRC =
	  Msg !Channel !String
	| Notice !Nocase !String

	| Join !Channel !String
	| Part !Channel !String
	| Quit !String
	| Kick !Channel !Nocase !String

	| Nick !Nocase
	| UserName !String !String
	| Password !String

	| Ping !String
	| Pong !String

	| UserList !Channel [(Nocase, Status)]
	| NickInUse !Nocase !Nocase
	| Welcome !Nocase !String

	| Raw !String ![String]


instance Eq Name where
	Name a b c == Name a2 b2 c2 = f a a2 && f b b2 && f c c2 where
		f x y = x == Nocase "*" || y == Nocase "*" || x == y

instance Show Name where
	show (Name (Nocase a) (Nocase b) (Nocase c)) = a ++ "!" ++ b ++ "@" ++ c

instance Show Sender where
	show (NUH x)			= show x
	show (Server (Nocase s))	= s
	show _				= ""

p353toTuples :: String -> [(Nocase, Status)]
p353toTuples = map match . words where
	match ('@':n)	= (Nocase n, OP)
	match ('+':n)	= (Nocase n, Voice)
	match n		= (Nocase n, Normal)

ircToMessage :: String -> Maybe Message
ircToMessage = either (const Nothing) (Just) . parse toMessage []

responseToIrc :: IRC -> String
responseToIrc x = case x of
	Msg	(Nocase c) m		-> "PRIVMSG " ++ c ++ " :" ++ case stripPrefixWith toLower "/me " m of
						Nothing	->  m
						Just a	-> "\1ACTION " ++ a ++ "\1"
	Notice	(Nocase c) m		-> "NOTICE " ++ c ++ " :" ++ m
	Nick	(Nocase m)		-> "NICK " ++ m
	UserName u n			-> "USER " ++ u ++ " 0 * :" ++ n
	Password m			-> "PASS " ++ m
	Join 	(Nocase c) pass		-> "JOIN " ++ c ++ " :" ++ pass
	Part	(Nocase c) m		-> "PART " ++ c ++" :" ++ m
	Kick	(Nocase c) (Nocase w) m	-> "KICK " ++ c ++ " " ++ w ++ " :" ++ m
	Ping	m			-> "PING :" ++ m
	Pong	m			-> "PONG :" ++ m
	Quit	m			-> "QUIT :" ++ m
	Raw	c _			-> c
	_				-> ""

commandMap :: String -> [String] -> IRC
commandMap action items = case (action, items) of
	("PRIVMSG"	, [c,msg])	-> Msg (Nocase c) msg
	("NOTICE"	, [c,msg])	-> Notice (Nocase c) msg
	("KICK"	, [c,w,msg])		-> Kick (Nocase c) (Nocase w) msg
	("353"	, [_,_,c,users])	-> UserList (Nocase c) (p353toTuples users)
	("JOIN"	, [c])			-> Join (Nocase c) ""
	("QUIT"	, [msg])		-> Quit msg
	("PART"	, [c, msg])		-> Part (Nocase c) msg
	("NICK"	, [new])		-> Nick (Nocase new)
	("433"	, [current, requested])	-> NickInUse (Nocase current) (Nocase requested)
	("PING"	, [msg])		-> Ping msg
	("PONG"	, [msg])		-> Pong msg
	("001"	, [nick, msg])		-> Welcome (Nocase nick) msg
	(a	, b)			-> Raw a b

readNUH :: String -> Either ParseError Name
readNUH = parse nuh []

toMessage :: GenParser Char st Message
toMessage = do
	first	<- try (char ':' >> getSender) <|> return NoSender
	spaces
	command <- many1 toSpace
	args	<- many (spaces >> getArgs)
	eof
	return $ Message first (commandMap command args)

getArgs :: GenParser Char st String
getArgs = (char ':' >> many anyChar) <|> (many1 toSpace)

getSender, server :: GenParser Char st Sender
getSender = (NUH `liftM` try nuh <|> server)

nuh :: GenParser Char st Name
nuh = do
	n <- many1 $ satisfy (/='!')
	char '!'
	u <- many1 $ satisfy (/='@')
	char '@'
	h <- many1 toSpace
	return $ Name (Nocase n) (Nocase u) (Nocase h)

server = (Server . Nocase) `liftM` many1 toSpace

toSpace :: CharParser st Char
toSpace	= satisfy (not . isSpace)
