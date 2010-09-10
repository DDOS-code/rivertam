module Tremulous.Protocol(
	Team(..), CVar, ServerInfo(..), PlayerInfo(..), MasterInfo(..)
	, cycleoutIP, pollFormat
)where
import Control.DeepSeq
import Data.Bits
import Data.Maybe
import Text.Read
import Control.Monad
import Network.Socket
import Helpers


data Team = Spectators | Aliens | Humans | Unknown deriving (Eq, Show)

readTeam :: Char -> Team
readTeam x = case x of
	'0'	-> Spectators
	'1'	-> Aliens
	'2'	-> Humans
	_	-> Unknown

type CVar = (Nocase, String)

data ServerInfo = ServerInfo {
	address		:: !SockAddr
	, origin	:: !(Maybe MasterInfo)
	, cvars		:: ![CVar]
	, players	:: ![PlayerInfo]
	}

data PlayerInfo	= PlayerInfo {
	  team	:: !Team
	, kills
	, ping	:: !Int
	, name	:: !String 
	}

data MasterInfo = MasterInfo {
	  mident	:: !String
	, protocol	:: !Int
	, masterHost	:: !SockAddr
	} deriving Eq

instance (Read PlayerInfo) where
	readPrec = do
		Int kills	<- lexP
		Int ping	<- lexP
		String name	<- lexP
		return $ PlayerInfo Unknown (fromInteger kills) (fromInteger ping) name

instance NFData MasterInfo where
	rnf (MasterInfo a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData Team

instance NFData ServerInfo where
	rnf (ServerInfo a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance NFData PlayerInfo where
	rnf (PlayerInfo a b c d)  = rnf a `seq` rnf b `seq` rnf c `seq` rnf d


deriving instance Ord SockAddr

instance NFData SockAddr where
	rnf (SockAddrInet (PortNum p) h) 	= rnf p `seq` rnf h
	rnf (SockAddrInet6 (PortNum p) f h s)	= rnf p `seq` rnf f `seq` rnf h `seq` rnf s
	rnf (SockAddrUnix s)			= rnf s

cycleoutIP :: String -> [SockAddr]
cycleoutIP ('\\' :'E':'O':'T':'\0':'\0':'\0':[]) = []
cycleoutIP ('\\' : i0:i1:i2:i3 : p0:p1 : xs) = SockAddrInet port ip : cycleoutIP xs
	where	ip	= fromIntegral $ (ord i3 .<<. 24) .|. (ord i2 .<<. 16) .|. (ord i1 .<<. 8) .|. ord i0
		port	= fromIntegral $ (ord p0 .<<. 8) .|. ord p1
		(.<<.)	= shiftL
cycleoutIP _ = []

pollFormat :: SockAddr -> String -> Maybe ServerInfo
pollFormat s line = case splitlines line of
		(cvars_:players_) -> let
			cvars	= cvarstuple . split (=='\\') $ cvars_
			players	= case lookup (Nocase "P") cvars of
				Nothing -> mapMaybe mread players_ 
				Just a	-> playerList players_ a
			in Just $ ServerInfo s Nothing cvars players
		_ -> Nothing

playerList :: [String] -> [Char] -> [PlayerInfo]
playerList []		_	= []
playerList (p:ps)	[]	= mread p /: playerList ps []
playerList ps		('-':ls)= playerList ps ls
playerList (p:ps)	(l:ls)	= (\x -> x {team = readTeam l}) `liftM` mread p /: playerList ps ls

cvarstuple :: [String] -> [CVar]
cvarstuple (c:v:ss)	= (Nocase c, v) : cvarstuple ss
cvarstuple _		= []

(/:) :: Maybe t -> [t] -> [t]
(Just x) /: xs = x:xs
Nothing /: xs = xs
