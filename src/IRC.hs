-- The parsing part of this module is inspired by Network.IRC, written by
-- Trevor Elliott, http://hackage.haskell.org/cgi-bin/hackage-scripts/package/irc
module IRC (
	  Status(..)
	, Domain(..)
	, Sender(..)
	, Message(..)
	, Response(..)
	, ircToMessage
	, responseToIrc
	, readNUH
	, p353toTuples
) where
import Text.ParserCombinators.Parsec
import Helpers
import Control.Monad


data Status = Normal | Voice | OP deriving (Show, Eq, Ord)

infixr 3 :@
infixl 2 :!

data Domain a = !a :@ !a

data Sender = !Nocase :! !(Domain Nocase) | Server !Nocase | NoSender

data Message = Message !(Maybe Sender) !String [String]

data Response =
	  Msg !Nocase !String
	| Me !Nocase !String
	| Join !Nocase !String
	| Part !Nocase !String
	| Notice !Nocase !String
	| Nick !Nocase
	| UserName !String !String
	| Kick !String !String
	| Pong !String
	| Quit !String
	| Hijack !String
	--deriving Show

instance Eq Sender where
	(a :! b :@ c) == (a2 :! b2 :@ c2) = f a a2 && f b b2 && f c c2 where
		f x y = x == Nocase "*" || y == Nocase "*" || x == y
	_ == _ = False

instance Show Sender where
	show (Nocase a :! Nocase b :@ Nocase c)	= a ++ "!" ++ b ++ "@" ++ c
	show (Server (Nocase s))			= s
	show _						= ""

p353toTuples :: String -> [(Nocase, Status)]
p353toTuples = map match . words where
	match ('@':n)	= (Nocase n, OP)
	match ('+':n)	= (Nocase n, Voice)
	match n		= (Nocase n, Normal)

ircToMessage :: String -> Maybe Message
ircToMessage = either (const Nothing) Just . parse toMessage []

responseToIrc :: Response -> String
responseToIrc x = case x of
	Msg	(Nocase c) m		-> "PRIVMSG " ++ c ++ " :" ++ m
	Me	(Nocase c) m		-> "PRIVMSG "++c++" :\1ACTION "++m++"\1"
	Notice	(Nocase c) m		-> "NOTICE "++c++" :"++m
	Nick	(Nocase m)		-> "NICK " ++ m
	UserName u n			-> "USER " ++ u ++ " 0 * :" ++ n
	Join 	(Nocase c) pass	-> "JOIN " ++ c ++ " :" ++ pass
	Part	(Nocase c) m		-> "PART "++c++" :"++m
	Kick	c m			-> "KICK "++c++" "++m
	Pong	m			-> "PONG :" ++ m
	Quit	m			-> "QUIT :" ++ m
	Hijack m			-> m


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
	return $ Nocase n :! Nocase u :@ Nocase h

server = (Server . Nocase) `liftM` many1 toSpace

toSpace :: CharParser st Char
toSpace	= satisfy (not . isSpace)
