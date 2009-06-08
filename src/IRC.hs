-- The parsing part of this module is inspired by Network.IRC, written by
-- Trevor Elliott, http://hackage.haskell.org/cgi-bin/hackage-scripts/package/irc
module IRC (
	  Status(..)
	, Sender(..)
	, Message(..)
	, Response(..)
	, ircToMessage
	, responseToIrc
	, readNUH
	, p353toTuples
) where
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad


data Status = Normal | Voice | OP deriving (Show, Eq, Ord)

data Sender = NUH !String !String !String | Server !String

data Message = Message !(Maybe Sender) !String [String]

data Response =
	  Msg !String !String
	| Me !String !String
	| Join !String !String
	| Part !String !String
	| Notice !String !String
	| Nick !String
	| UserName !String !String
	| Kick !String !String
	| Pong !String
	| Hijack !String
	deriving Show

instance Eq Sender where
	(NUH a b c) == (NUH a2 b2 c2) = f a a2 && f b b2 && f c c2 where
		f x y = if x == "*" || y == "*"	then True else map toLower x == map toLower y
	_ == _ = False

instance Show Sender where
	show (NUH a b c)	= a ++ "!" ++ b ++ "@" ++ c
	show (Server s)		= s

p353toTuples :: String -> [(String, Status)]
p353toTuples = map match . words . map toLower where
	match ('@':n)	= (n, OP)
	match ('+':n)	= (n, Voice)
	match n		= (n, Normal)

ircToMessage :: String -> Maybe Message
ircToMessage = either (const Nothing) Just . parse toMessage []

responseToIrc :: Response -> String
responseToIrc x = case x of
	Msg	c m	-> "PRIVMSG " ++ c ++ " :" ++ m
	Me	c m	-> "PRIVMSG "++c++" :\1ACTION "++m++"\1"
	Notice	c m	-> "NOTICE "++c++" :"++m
	Nick	m	-> "NICK " ++ m
	UserName u n	-> "USER " ++ u ++ " 0 * :" ++ n
	Join 	c pass	-> "JOIN " ++ c ++ " :" ++ pass
	Part	c m	-> "PART "++c++" :"++m
	Kick	c m	-> "KICK "++c++" "++m
	Pong	m	-> "PONG :" ++ m
	Hijack m	-> m


readNUH :: String -> Either ParseError Sender
readNUH = parse nuh []

toMessage :: GenParser Char st Message
toMessage = do
	first	<- optionMaybe (char ':' >> getSender)
	spaces
	command <- many1 toSpace
	args	<- many (spaces >> getArgs)
	eof
	return $ Message first command args

getArgs :: GenParser Char st String
getArgs = (char ':' >> many anyChar) <|> (many1 toSpace)

getSender, nuh, server :: GenParser Char st Sender
getSender = (try nuh <|> server)

nuh = do
	n <- many1 $ satisfy (/='!')
	char '!'
	u <- many1 $ satisfy (/='@')
	char '@'
	h <- many1 toSpace
	return $ NUH n u h

server = do
	x <- many1 toSpace
	return $ Server x

toSpace :: CharParser st Char
toSpace	= satisfy (not . isSpace)
