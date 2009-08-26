module IrcState (
	  IrcState(..)
	, ircUpdate
) where
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

import Helpers
import IRC

type IrcMap = Map Nocase (Map Nocase Status)

data IrcState = IrcState {
	  ircNick	:: !Nocase
	, ircMap	:: !IrcMap
	}

nMap :: IrcState -> (IrcMap -> IrcMap) -> IrcState
nMap s@IrcState{ircMap} f = s{ircMap = f ircMap}

ircUpdate :: Message -> IrcState -> IrcState
ircUpdate (Message x com val) state = update x com val state

update :: Sender -> String -> [String] -> IrcState -> IrcState

update  _ "KICK" (chan'':kicked'':_) state@IrcState{ircNick} = let
	chan	= Nocase chan''
	kicked	= Nocase kicked''
	f	= if ircNick == kicked
			then M.delete chan
			else M.adjust (M.delete kicked) chan
	in nMap state f

-- User list
--":kornbluth.freenode.net 353 river-tam = ##ddos :river-tam @stoned_es Spartakusafk @raf_kig @Cadynum @Saliva Fleurka @PhilH @ChanServ"
update _ "353" [_,_,chan'',users''] state = let
	chan		= Nocase chan''
	users		= p353toTuples users''
	newchanmap	= M.singleton chan (M.fromList users)
	in nMap state $ M.unionWith M.union newchanmap

update (NUH (Name nick _ _)) "JOIN" [chan] state =
	nMap state $ M.insertWith M.union (Nocase chan) (M.singleton nick Normal)

update (NUH (Name nick _ _)) "QUIT" _ state = nMap state $ M.map (M.delete nick)

--":Cadynum!n=cadynum@unaffiliated/cadynum PART ##ddos :\"Moo!\""
update (NUH (Name nick _ _)) "PART" (chan'':_) state@IrcState{ircNick} = let
	chan	= Nocase chan''
	f	= if ircNick == nick
			then M.delete chan
			else M.adjust (M.delete nick) chan
	in nMap state f

--":JoKe|!i=joke@lyseo.edu.ouka.fi NICK :JoKe|hungry"
update (NUH (Name nick _ _)) "NICK" [changedNick''] state@IrcState{ircNick, ircMap} = let
	changedNick	= Nocase changedNick''
	newMap		= M.map (modifyKey nick changedNick) ircMap
	newNick		= if nick == ircNick then changedNick else ircNick
	in state{ircMap = newMap, ircNick = newNick}

--":kornbluth.freenode.net 001 river-tam|30 :Welcome to the freenode IRC Network river-tam|30"
update _ "001" (mynick:_) state@IrcState{ircNick = Nocase ""} =
	state{ircNick = Nocase mynick}

update _ _ _ s = s

modifyKey :: (Ord k) => k -> k -> Map k a -> Map k a
modifyKey old new m = case M.lookup old m of
	Nothing	-> m
	Just a	-> M.insert new a . M.delete old $ m
