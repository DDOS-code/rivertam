module Irc.OnEvent (init, respond, updateConfig) where
import Prelude hiding (init)
import Data.Maybe
import Data.Foldable
import Data.List ((\\))
import qualified Data.Map as M

import Config
import Helpers
import Irc.Protocol
import Irc.State

init :: Config -> [IRC]
init Config {name, user, nick, password} = (if null password then id else (Password password:)) [UserName user name, Nick nick]

updateConfig :: Config -> IrcState -> [IRC]
updateConfig config IrcState{ircNick, ircMap} = let
		lnick	= if recase (nick config) == recase ircNick then [] else [Nick (nick config)]
		lpart	= map (`Part` "over and out") $ oldchans \\ newchans
		ljoin	= map (\x -> Join x (maybePass x)) $ newchans \\ oldchans

		maybePass x	= maybe "" id $ lookup x (channels config)
		newchans	= map fst $ channels config
		oldchans	= M.keys ircMap
	in lnick ++ lpart ++ ljoin



respond :: Config -> Message -> IrcState -> [IRC]
respond
	Config{channels, access, nick}
	(Message sender command)
	IrcState{ircNick}
	= case (command, sender) of

	(Kick c who _	, NUH (Name evil _ _))
		| who == ircNick -> let pass = fromMaybe "" (lookup c channels)
		in [Join c pass, Msg c (recase evil ++ ", thanks very much for the kick!")]

	(Notice who msg	, NUH nuh)
		| (ircNick == who && getAccess nuh access == Master) -> [Raw msg []]

	(Welcome _ _	, Server _) -> map (uncurry Join) channels

	-- ONLY send for a new nick in case we don't already have a nick
	-- ":kornbluth.freenode.net 433 river-tam59 river-tam :Nickname is already in use."
	-- ":grisham.freenode.net 433 * staxie :Nickname is already in use."
	(NickInUse (Nocase "*") _, Server _) ->
		[Nick (Nocase (take 14 (recase nick) ++ "_"))]

	(Ping msg	, NoSender) ->
		[Pong msg]

	_ -> []

getAccess :: Name -> [(Access, Name)] -> Access
getAccess who access = maybe Peon fst $ find (\(_, n) -> n == who) access
