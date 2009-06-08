module Parse (
	  IrcState(..)
	, External(..)
	, initIRC
	, updateConfig
	, parseMode
) where
import qualified Data.Map as M
import System.Random
import Control.Monad.State
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

import Config
import Helpers
import IRC

data External = ExecCommand !Access !String !String !String

data IrcState = IrcState {
	  ircNick	:: !String
	, ircMap	:: !(Map String (Map String Status))
	, memos		:: !(Map String (Seq (Sender, String)))
	}

type ParseReturn = State IrcState ([Response], Maybe External)

initIRC :: Config -> State IrcState [Response]
initIRC Config {name, user, nick} = return $
	[ UserName user name
	, Nick nick
	]

updateConfig :: Config -> State IrcState [Response]
updateConfig config = do
	ircNick	<- gets ircNick
	ircMap	<- gets ircMap

	let	lnick	= if (nick config) =|= ircNick then [] else [Nick (nick config)]
		lpart	= map (`Part` "over and out")  $ oldchans \\ newchans
		ljoin	= map (\x -> Join x (maybePass x)) $ newchans \\ oldchans

		maybePass x	= maybe "" id $ lookup x (channels config)
		newchans	= map (map toLower . fst) $ channels config
		oldchans	= M.keys ircMap

	return $ lnick ++ lpart ++ ljoin


parseMode :: Config -> Message ->  ParseReturn
parseMode Config{comkey, access, queryaccess} (Message (Just prefix@(NUH sender _ _)) "PRIVMSG" (reciever:mess:[])) = do
	ircNick			<- gets ircNick
	let	cPrefixes	= [comkey, ircNick++", ", ircNick++": "]
		gotaccess	= getAccess prefix access

	memos	<- sendMemos (map toLower sender)

	let com	= case findprefix cPrefixes mess of
		Just a | not $ ircNick =|= reciever
			-> Just $ ExecCommand gotaccess reciever sender a
		Just a | gotaccess >= queryaccess
			-> Just $ ExecCommand gotaccess sender sender a
		_	-> Nothing
	return (memos, com)

parseMode Config{channels} (Message (Just (NUH sender _ _)) "KICK" (chan_:kickedPerson:_)) = do
	ircNick			<- gets ircNick
	ircMap			<- gets ircMap
	let	chan		= map toLower chan_
		pass		= fromMaybe [] $ lookup (map toLower chan) channels
		ircMap'		= if ircNick =|= kickedPerson then M.delete (map toLower chan) ircMap
					else M.adjust (M.delete (map toLower kickedPerson)) (map toLower chan) ircMap

	modify $ \x -> x {ircMap = ircMap'}

	returnI $ if ircNick =|= kickedPerson then
		[ Join chan pass
		, Msg chan $ sender++", thanks very much for the kick!"
		] else []

--":Cadynum-Pirate!n=cadynum@unaffiliated/cadynum NOTICE river-tam|pirate :test"
parseMode Config{access} (Message (Just nuh) "NOTICE" (s2:s3:[])) = do
	ircNick		<- gets ircNick
	let gotaccess	= getAccess nuh access
	returnI $ if ircNick =|= s2 &&  gotaccess == Master
		then [Hijack s3]
		else []

-- User list
--":kornbluth.freenode.net 353 river-tam = ##ddos :river-tam @stoned_es Spartakusafk @raf_kig @Cadynum @Saliva Fleurka @PhilH @ChanServ"
parseMode _ (Message (Just _) "353" (_:_:a:b:[])) = do
	ircMap <- gets ircMap
	let	newchanmap = M.singleton chan (M.fromList users)
		woo = M.unionWith M.union newchanmap ircMap
	modify $ \x -> x {ircMap=woo}
	returnI []
	where	chan	= map toLower a
		users	= p353toTuples b

parseMode _ (Message (Just (NUH nick_ _ _)) "JOIN" [chan_]) = do
	ircMap		<- gets ircMap
	let foo = M.insertWith M.union chan (M.singleton nick Normal) ircMap
	modify $ \x -> x {ircMap=foo}

	--Send any memos if there are any
	returnI =<< sendMemos nick

	where 	nick	= map toLower nick_
		chan	= map toLower chan_

parseMode _ (Message (Just (NUH nick_ _ _)) "QUIT" _) = do
	ircMap		<- gets ircMap
	let nmap	= M.map (M.delete nick) ircMap
	modify $ \x -> x {ircMap=nmap}
	returnI []
	where	nick = map toLower nick_


--":Cadynum!n=cadynum@unaffiliated/cadynum PART ##ddos :\"Moo!\""
parseMode _ (Message (Just (NUH nick_ _ _)) "PART" (chan_:_)) = do
	ircMap		<- gets ircMap
	ircNick		<- gets ircNick
	let ircMap'	= if ircNick =|= nick then M.delete chan ircMap
			 	else M.adjust (M.delete nick) chan ircMap
	modify $ \x -> x {ircMap=ircMap'}
	returnI []
	where	nick	= map toLower nick_
		chan	= map toLower chan_

--":JoKe|!i=joke@lyseo.edu.ouka.fi NICK :JoKe|hungry"
parseMode _ (Message (Just (NUH nick_ _ _)) "NICK" (c:[])) = do
	ircMap			<- gets ircMap
	ircNick			<- gets ircNick
	let	nick		= map toLower nick_
		newnick		= map toLower c
		nmap		= M.map (modifyKey nick newnick) ircMap
	modify $ \x -> x {ircMap=nmap}

	when (nick =|= ircNick) $ do
		modify $ \x -> x {ircNick=newnick}
	returnI []

--":kornbluth.freenode.net 001 river-tam|30 :Welcome to the freenode IRC Network river-tam|30"
parseMode Config{nickserv} (Message (Just _) "001" (mynick:_)) = do
	ircNick			<- gets ircNick
	when (null ircNick) $ do
		modify $ \x -> x {ircNick=mynick}

	returnI $ if null nickserv then [] else
		[Msg "NickServ" ("IDENTIFY "++nickserv)]


-- ONLY send for a new nick in case we don't already have a nick
-- ":kornbluth.freenode.net 433 river-tam59 river-tam :Nickname is already in use."
-- ":grisham.freenode.net 433 * staxie :Nickname is already in use."
parseMode Config{nick} (Message (Just _) "433" ("*":_)) = do
	let 	rand	= fst $ randomR (0, 100::Int) (mkStdGen 100) --VERY TMP, not random
		newnick	= (take 10 nick) ++ show rand
	returnI [Nick newnick]

--Nickserv signed in.
parseMode Config{nickserv, channels} (Message (Just _) "901" _) = do
	returnI $ if null nickserv then [] else
		map (\(chan, pass) -> Join chan pass) channels

--Or end of motd
parseMode Config{nickserv, channels} (Message (Just _) "376" _) = do
	returnI $ if not $ null nickserv then [] else
		map (\(chan, pass) -> Join chan pass) channels

parseMode _ (Message Nothing "PING" _) = returnI [ Pong "Ayekarambaa" ]

parseMode _ _ = return ([], Nothing)

sendMemos :: String -> State IrcState [Response]
sendMemos nick = do
	memos		<- gets memos
	--Send any memos if there are any
	case M.lookup nick memos of
		Nothing	-> return []
		Just a	-> do
			modify $ \x -> x {memos = M.delete nick memos}
			return $ map (\(from,mess) -> Msg nick (show from ++ ": " ++ mess)) (F.toList a)

returnI :: [Response] -> ParseReturn
returnI x = return (x, Nothing)

{-
returnE :: Maybe External -> ParseReturn
returnE x = return ([], x)
-}

modifyKey :: (Ord k) => k -> k -> Map k a -> Map k a
modifyKey old new m = case M.lookup old m of
	Nothing	-> m
	Just a	-> M.insert new a . M.delete old $ m


findprefix :: [String] -> String -> Maybe String
findprefix	[]	_	= Nothing
findprefix	(x:xs)	input	= case shavePrefixWith toLower x input of
					Nothing -> findprefix xs input
					a	-> a

getAccess :: Sender -> [(Access, Sender)] -> Access
getAccess who access = case find (\(_, n) -> n == who) access of
		Nothing -> Peon
		Just (a, _) -> a
