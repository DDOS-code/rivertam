-- The parsing part of this module is inspired by Network.IRC, written by
-- Trevor Elliott, http://hackage.haskell.org/cgi-bin/hackage-scripts/package/irc
module IRC (
	  Status(..)
	, Sender(..)
	, Message(..)
	, ircToMessage
	, readNUH
	, p353toTuples
) where
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad


data Status = Normal | Voice | OP deriving (Show, Eq, Ord)

data Sender = NUH !String !String !String | Server !String

data Message = Message !(Maybe Sender) !String [String]

instance Eq Sender where
	(NUH a b c) == (NUH a2 b2 c2) = f a a2 && f b b2 && f c c2 where
		f x y = if x == "*" || y == "*"	then True else map toLower x == map toLower y
	_ == _ = False

p353toTuples :: String -> [(String, Status)]
p353toTuples = map match . words . map toLower where
	match ('@':n)	= (n, OP)
	match ('+':n)	= (n, Voice)
	match n		= (n, Normal)

ircToMessage :: String -> Maybe Message
ircToMessage = either (const Nothing) Just . parse toMessage []

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
