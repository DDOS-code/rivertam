module Parse (
	  IrcState(..)
	, External(..)
	, initIRC
	, updateConfig
	, parse
) where
import qualified Data.Map as M
import Data.Maybe
import Data.Foldable
import Data.List ((\\))

import Config
import Helpers
import IRC
import IrcState

data External = ExecCommand !Access !Caseless !Caseless !String | BecomeActive !Caseless


type ParseReturn = ([Response], [External])

initIRC :: Config -> [Response]
initIRC Config {name, user, nick} = [UserName user name, Nick nick]

updateConfig :: Config -> IrcState -> [Response]
updateConfig config IrcState{ircNick, ircMap} = let
		lnick	= if decase (nick config) == decase ircNick then [] else [Nick (nick config)]
		lpart	= map (`Part` "over and out") $ oldchans \\ newchans
		ljoin	= map (\x -> Join x (maybePass x)) $ newchans \\ oldchans

		maybePass x	= maybe "" id $ lookup x (channels config)
		newchans	= map fst $ channels config
		oldchans	= M.keys ircMap
	in lnick ++ lpart ++ ljoin


parse :: Config -> IrcState -> Message -> ParseReturn
parse Config{comkey, access, queryaccess} IrcState{ircNick} (Message (Just prefix@(sender :! _)) "PRIVMSG" [reciever, msg]) = let
	cPrefixes	= [comkey, decase ircNick++", ", decase ircNick++": "]
	gotaccess	= getAccess prefix access
	reciever'	= Caseless reciever

	com	= case findprefix cPrefixes msg of
		Just a	| ircNick /= reciever' ->
				[ExecCommand gotaccess reciever' sender a]
			| gotaccess >= queryaccess ->
				[ExecCommand gotaccess sender sender a]
		_ 	-> []
	in ([], (BecomeActive sender):com)


parse Config{channels} IrcState{ircNick} (Message (Just (sender :! _ )) "KICK" (chan'':kicked:_)) = let
	chan 	= Caseless chan''
	pass	= fromMaybe [] $ lookup chan channels
	in mess $ if ircNick == Caseless kicked then
		[ Join chan pass
		, Msg chan $ decase sender ++ ", thanks very much for the kick!"
		] else []

--":Cadynum-Pirate!n=cadynum@unaffiliated/cadynum NOTICE river-tam|pirate :test"
parse Config{access} IrcState{ircNick} (Message (Just nuh) "NOTICE" [s2, s3]) =
	mess $ if ircNick == Caseless s2 && getAccess nuh access == Master
		then [Hijack s3]
		else []

parse _ _ (Message (Just (nick :! _)) "JOIN" _) = ([], [BecomeActive nick])
parse _ _ (Message (Just (nick :! _)) "NICK" _) = ([], [BecomeActive nick])

parse Config{nickserv} _ (Message (Just _) "001" _) =
	mess $ if null nickserv then [] else
		[Msg (Caseless "NickServ") ("IDENTIFY " ++ nickserv)]

-- ONLY send for a new nick in case we don't already have a nick
-- ":kornbluth.freenode.net 433 river-tam59 river-tam :Nickname is already in use."
-- ":grisham.freenode.net 433 * staxie :Nickname is already in use."
parse Config{nick=(Caseless nick)} _ (Message (Just _) "433" ("*":_)) =
	mess . (:[]) . Nick . Caseless $ take 14 nick ++ "_"

--Nickserv signed in.
parse Config{nickserv=(_:_), channels} _ (Message (Just _) "901" _) =
	mess $ map (uncurry Join) channels

--Or end of motd
parse Config{nickserv=[], channels} _ (Message (Just _) "376" _) =
	mess $ map (uncurry Join) channels


parse _ _ (Message Nothing "PING" [x]) = mess [Pong x]

parse _ _ _ = ([], [])


mess :: [Response] -> ParseReturn
mess x = (x, [])


findprefix :: [String] -> String -> Maybe String
findprefix	[]	_	= Nothing
findprefix	(x:xs)	input	= case shavePrefixWith toLower x input of
					Nothing -> findprefix xs input
					a	-> a

getAccess :: Sender -> [(Access, Sender)] -> Access
getAccess who access = maybe Peon fst $ find (\(_, n) -> n == who) access
