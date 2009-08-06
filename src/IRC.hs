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

data Sender = !Caseless :! !(Domain Caseless) | Server !Caseless | NoSender

data Message = Message !(Maybe Sender) !String [String]

data Response =
	  Msg !Caseless !String
	| Me !Caseless !String
	| Join !Caseless !String
	| Part !Caseless !String
	| Notice !Caseless !String
	| Nick !Caseless
	| UserName !String !String
	| Kick !String !String
	| Pong !String
	| Quit !String
	| Hijack !String
	--deriving Show

instance Eq Sender where
	(a :! b :@ c) == (a2 :! b2 :@ c2) = f a a2 && f b b2 && f c c2 where
		f x y = x == Caseless "*" || y == Caseless "*" || x == y
	_ == _ = False

instance Show Sender where
	show (Caseless a :! Caseless b :@ Caseless c)	= a ++ "!" ++ b ++ "@" ++ c
	show (Server (Caseless s))			= s
	show _						= ""

p353toTuples :: String -> [(Caseless, Status)]
p353toTuples = map match . words where
	match ('@':n)	= (Caseless n, OP)
	match ('+':n)	= (Caseless n, Voice)
	match n		= (Caseless n, Normal)

ircToMessage :: String -> Maybe Message
ircToMessage = either (const Nothing) Just . parse toMessage []

responseToIrc :: Response -> String
responseToIrc x = case x of
	Msg	(Caseless c) m			-> "PRIVMSG " ++ c ++ " :" ++ m
	Me	(Caseless c) m			-> "PRIVMSG "++c++" :\1ACTION "++m++"\1"
	Notice	(Caseless c) m		-> "NOTICE "++c++" :"++m
	Nick	(Caseless m)		-> "NICK " ++ m
	UserName u n			-> "USER " ++ u ++ " 0 * :" ++ n
	Join 	(Caseless c) pass	-> "JOIN " ++ c ++ " :" ++ pass
	Part	(Caseless c) m		-> "PART "++c++" :"++m
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
	return $ Caseless n :! Caseless u :@ Caseless h

server = (Server . Caseless) `liftM` many1 toSpace

toSpace :: CharParser st Char
toSpace	= satisfy (not . isSpace)
