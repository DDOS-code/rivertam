module Irc.State (
	  IrcState(..), initial, update
) where
import qualified Data.Map as M
import Data.Map (Map)

import Helpers
import Irc.Protocol

type IrcMap = Map Nocase (Map Nocase Status)

data IrcState = IrcState {
	  ircNick	:: !Nocase
	, ircMap	:: !IrcMap
	}

initial :: IrcState
initial = IrcState{ ircNick = Nocase "", ircMap = M.empty }

update :: Message -> IrcState -> IrcState
update (Message first irc) state@IrcState{ircMap, ircNick} = case (irc, first) of
	(Kick c who _		, _) ->
		newMap $ if ircNick == who then M.delete c else M.adjust (M.delete who) c

	(UserList c users	, Server _) ->
		newMap $ M.unionWith M.union (M.singleton c (M.fromList users))

	(Join c _		, (NUH (Name nick _ _))) ->
		newMap $ M.insertWith M.union c (M.singleton nick Normal)

	(Quit _			, (NUH (Name nick _ _))) ->
		newMap $ M.map (M.delete nick)

	(Part c  _		, (NUH (Name nick _ _))) ->
		newMap $ if ircNick == nick then M.delete c else M.adjust (M.delete nick) c

	(Nick new		, (NUH (Name nick _ _))) -> let
		ircNick'	= if nick == ircNick then new else ircNick
		ircMap'		= M.map (modifyKey nick new) ircMap
		in state {ircNick = ircNick', ircMap = ircMap'}

	(Welcome new _, _) ->
		state {ircNick = new}

	_		-> state
	where
	newMap f = state {ircMap=f ircMap}

modifyKey :: (Ord k) => k -> k -> Map k a -> Map k a
modifyKey old new m = case M.lookup old m of
	Nothing	-> m
	Just a	-> M.insert new a . M.delete old $ m
