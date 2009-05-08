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

import Config
import Helpers
import IRC

data External = ExecCommand !Access !String !String !String

data IrcState = IrcState {
	  config	:: !Config
	, ircNick	:: !String
	, ircMap	:: !(Map String (Map String Status))
	}

type ParseReturn = State IrcState ([Response], Maybe External)

initIRC :: State IrcState [Response]
initIRC = do
	Config {name, user, nick} <- gets config
	--Send user & nick info
	return $
		[ UserName user name
		, Nick nick
		]

updateConfig :: State IrcState [Response]
updateConfig = do
	config	<- gets config
	ircNick	<- gets ircNick
	ircMap	<- gets ircMap

	let	lnick	= if (nick config) =|= ircNick then [] else [Nick (nick config)]
		lpart	= map (`Part` "over and out")  $ oldchans \\ newchans
		ljoin	= map (\x -> Join x (maybePass x)) $ newchans \\ oldchans

		maybePass x	= maybe "" id $ lookup x (channels config)
		newchans	= map (map toLower . fst) $ channels config
		oldchans	= M.keys ircMap

	return $ lnick ++ lpart ++ ljoin


parseMode:: Message ->  ParseReturn
parseMode (Message (Just prefix@(NUH sender _ _)) "PRIVMSG" (reciever:mess:[])) = do
	ircNick				<- gets ircNick
	Config {comkey, alias}		<- gets config
	let	cPrefixes		= [comkey, ircNick++", ", ircNick++": "]
		checkAlias g s r x	= case shavePrefix comkey x of
						Nothing -> Just $ ExecCommand g s r x
						Just a	-> maybe Nothing (\a' -> Just $ ExecCommand g s r a') $ M.lookup (map toLower a) alias
	gotaccess			<- getAccess prefix

	returnE $ case findprefix cPrefixes mess of
		Just a | not $ ircNick =|= reciever
			-> checkAlias gotaccess reciever sender a
		Just a | gotaccess >= User
			-> checkAlias gotaccess sender sender a

		_	-> Nothing

parseMode (Message (Just (NUH sender _ _)) "KICK" (chan:kickedPerson:_)) = do
	Config {channels}	<- gets config
	ircNick			<- gets ircNick
	let pass		= fromMaybe [] $ lookup chan channels
	ircMap			<- gets ircMap
	let ircMap'		= if ircNick =|= kickedPerson then M.delete (map toLower chan) ircMap
					else M.adjust (M.delete (map toLower kickedPerson)) (map toLower chan) ircMap

	modify $ \x -> x {ircMap = ircMap'}

	returnI $ if ircNick =|= kickedPerson then
		[ Join chan pass
		, Msg chan $ sender++", thanks very much for the kick!"
		] else []

--":Cadynum-Pirate!n=cadynum@unaffiliated/cadynum NOTICE river-tam|pirate :test"
parseMode  (Message (Just nuh) "NOTICE" (s2:s3:[])) = do
	ircNick		<- gets ircNick
	gotaccess	<- getAccess nuh
	returnI $ if ircNick =|= s2 &&  gotaccess == Master
		then [Hijack s3]
		else []

-- User list
--":kornbluth.freenode.net 353 river-tam = ##ddos :river-tam @stoned_es Spartakusafk @raf_kig @Cadynum @Saliva Fleurka @PhilH @ChanServ"
parseMode (Message (Just _) "353" (_:_:a:b:[])) = do
	let	chan	= map toLower a
		users	= p353toTuples b

	ircMap <- gets ircMap
	let	newchanmap = M.singleton chan (M.fromList users)
		woo = M.unionWith M.union newchanmap ircMap
	modify $ \x -> x {ircMap=woo}
	returnI []

parseMode (Message (Just (NUH nick_ _ _)) "JOIN" [chan_]) = do
	let	nick	= map toLower nick_
		chan	= map toLower chan_
	ircMap		<- gets ircMap
	let foo = M.insertWith M.union chan (M.singleton nick Normal) ircMap
	modify $ \x -> x {ircMap=foo}
	returnI []

parseMode (Message (Just (NUH nick_ _ _)) "QUIT" _) = do
	let nick	= map toLower nick_
	ircMap		<- gets ircMap
	let nmap	= M.map (M.delete nick) ircMap
	modify $ \x -> x {ircMap=nmap}
	returnI []


--":Cadynum!n=cadynum@unaffiliated/cadynum PART ##ddos :\"Moo!\""
parseMode (Message (Just (NUH nick_ _ _)) "PART" (c:_)) = do
	let	nick	= map toLower nick_
		chan	= map toLower c
	ircMap		<- gets ircMap
	ircNick		<- gets ircNick
	let ircMap'	= if ircNick =|= nick then M.delete chan ircMap
			 	else M.adjust (M.delete nick) chan ircMap

	modify $ \x -> x {ircMap=ircMap'}
	returnI []

--":JoKe|!i=joke@lyseo.edu.ouka.fi NICK :JoKe|hungry"
parseMode (Message (Just (NUH nick_ _ _)) "NICK" (c:[])) = do
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
parseMode (Message (Just _) "001" (mynick:_)) = do
	Config {nickserv}	<- gets config
	ircNick			<- gets ircNick

	when (ircNick =|= "") $ do
		modify $ \x -> x {ircNick=mynick}

	returnI $ if null nickserv then [] else
		[Msg "NickServ" ("IDENTIFY "++nickserv)]


-- ONLY send for a new nick in case we don't already have a nick
-- ":kornbluth.freenode.net 433 river-tam59 river-tam :Nickname is already in use."
-- ":grisham.freenode.net 433 * staxie :Nickname is already in use."
parseMode (Message (Just _) "433" ("*":_)) = do
	Config {nick}		<- gets config
	let 	rand	= fst $ randomR (0, 100::Int) (mkStdGen 100) --VERY TMP, not random
		newnick	= (take 10 nick) ++ show rand
	returnI [Nick newnick]

--Nickserv signed in.
parseMode (Message (Just _) "901" _) = do
	Config {nickserv, channels} <- gets config
	returnI $ if null nickserv then [] else
		map (\(chan, pass) -> Join chan pass) channels

--Or end of motd
parseMode (Message (Just _) "376" _) = do
	Config {nickserv, channels} <- gets config
	returnI $ if not $ null nickserv then [] else
		map (\(chan, pass) -> Join chan pass) channels

parseMode (Message Nothing "PING" _) = returnI [ Pong "Ayekarambaa" ]

parseMode _ = return ([], Nothing)

returnI :: [Response] -> ParseReturn
returnI x = return (x, Nothing)

returnE :: Maybe External -> ParseReturn
returnE x = return ([], x)

modifyKey :: (Ord k) => k -> k -> Map k a -> Map k a
modifyKey old new m = case M.lookup old m of
	Nothing	-> m
	Just a	-> M.insert new a . M.delete old $ m


findprefix :: [String] -> String -> Maybe String
findprefix	[]	_	= Nothing
findprefix	(x:xs)	input	= case shavePrefixWith toLower x input of
					Nothing -> findprefix xs input
					a	-> a

getAccess :: Sender -> State IrcState Access
getAccess who = do
	Config {access}	<- gets config

	return $ case find (\(_, n) -> n == who) access of
		Nothing -> Peon
		Just (a, _) -> a
