module Parse (parseIrcLine) where
import qualified Data.Map as M
import System.Random

import ComTremRelay
import Config
import Send
import Helpers
import RiverState
import IRC
import Commands


parseIrcLine :: String -> RiverState
parseIrcLine = maybe (return ()) parseMode . ircToMessage

--Ircmode command parser
parseMode:: Message -> RiverState

parseMode (Message (Just prefix@(NUH sender _ _)) "PRIVMSG" (reciever:mess:[])) = do
	rivNick				<- gets rivNick
	Config {comkey, alias}		<- gets config
	return ()
	let
		cPrefixes		= [comkey, rivNick++", ", rivNick++": "]
	gotaccess			<- getAccess prefix
	let	invokeCommand to = whenJust (findprefix cPrefixes mess) $ \a ->
			case shavePrefix comkey a of --Is it an alias?
				Nothing -> command gotaccess (sender, to, a)
				Just a1 -> whenJust (M.lookup (map toLower a1) alias) $ \a2 ->
					command gotaccess (sender, to, a2)

		action	| not $ rivNick =|= reciever	= invokeCommand reciever
			| gotaccess >= User		= invokeCommand sender
			| otherwise			= return ()
	action
	ircToTrem reciever sender mess


parseMode (Message (Just (NUH sender _ _)) "KICK" (chan:kickedPerson:_)) = do
	Config {channels}	<- gets config
	rivNick			<- gets rivNick
	let pass		= fromMaybe [] $ lookup chan channels
	rivMap			<- gets rivMap

	modify $ \x -> x {rivMap=M.adjust (M.delete (map toLower kickedPerson)) (map toLower chan) rivMap}

	when (rivNick == kickedPerson) $ do
		Join chan >>> pass
		Msg chan >>> sender++", thanks very much for the kick!"

--":Cadynum-Pirate!n=cadynum@unaffiliated/cadynum NOTICE river-tam|pirate :test"
parseMode  (Message (Just nuh) "NOTICE" (s2:s3:[])) = do
	rivNick		<- gets rivNick
	gotaccess	<- getAccess nuh
	when (rivNick =|= s2 &&  gotaccess == Master) $ do
		Raw >>> s3

-- User list
--":kornbluth.freenode.net 353 river-tam = ##ddos :river-tam @stoned_es Spartakusafk @raf_kig @Cadynum @Saliva Fleurka @PhilH @ChanServ"
parseMode (Message (Just _) "353" (_:_:a:b:[])) = do
	let	chan	= map toLower a
		users	= p353toTuples b

	rivMap <- gets rivMap
	let	newchanmap = M.singleton chan (M.fromList users)
		woo = M.unionWith M.union newchanmap rivMap
	modify $ \x -> x {rivMap=woo}

parseMode (Message (Just (NUH nick_ _ _)) "JOIN" [chan_]) = do
	let	nick	= map toLower nick_
		chan	= map toLower chan_
	rivMap		<- gets rivMap
	let foo = M.insertWith M.union chan (M.singleton nick Normal) rivMap
	modify $ \x -> x {rivMap=foo}

parseMode (Message (Just (NUH nick_ _ _)) "QUIT" _) = do
	let nick	= map toLower nick_
	rivMap		<- gets rivMap
	let nmap	= M.map (M.delete nick) rivMap
	modify $ \x -> x {rivMap=nmap}


--":Cadynum!n=cadynum@unaffiliated/cadynum PART ##ddos :\"Moo!\""
parseMode (Message (Just (NUH nick_ _ _)) "PART" (c:_)) = do
	let	nick	= map toLower nick_
		chan	= map toLower c
	rivMap		<- gets rivMap
	let foo = M.adjust (M.delete nick) chan rivMap
	modify $ \x -> x {rivMap=foo}

--":JoKe|!i=joke@lyseo.edu.ouka.fi NICK :JoKe|hungry"
parseMode (Message (Just (NUH nick_ _ _)) "NICK" (c:[])) = do
	rivMap			<- gets rivMap
	rivNick			<- gets rivNick
	let	nick		= map toLower nick_
		newnick		= map toLower c
		nmap		= M.map (modifyKey nick newnick) rivMap
	modify $ \x -> x {rivMap=nmap}

	when (nick =|= rivNick) $ do
		modify $ \x -> x {rivNick=newnick}

--":kornbluth.freenode.net 001 river-tam|30 :Welcome to the freenode IRC Network river-tam|30"
parseMode (Message (Just _) "001" (mynick:_)) = do
	Config {nickserv} <- gets config
	rivNick			<- gets rivNick
	when (not $ null nickserv) $
		Msg "NickServ" >>> "IDENTIFY "++nickserv
	when (rivNick =|= "") $ do
		modify $ \x -> x {rivNick=mynick}


-- ":grisham.freenode.net 433 * staxie :Nickname is already in use."
parseMode (Message (Just _) "433" _) = do
	Config {nick}		<- gets config
	rivNick			<- gets rivNick

	unless (nick =|= rivNick) $ do
		rand		<- lift $ getStdRandom . randomR $ (0, 100::Int)
		let newnick	= (take 10 nick) ++ show rand
		Nick >>> newnick


--Nickserv signed in.
parseMode (Message (Just _) "901" _) = do
	Config {nickserv, channels} <- gets config
	when (length  nickserv >= 1) $
		mapM_ (\(chan, pass) -> Join chan >>> pass) channels

--Or end of motd
parseMode (Message (Just _) "376" _) = do
	Config {nickserv, channels} <- gets config
	unless (length  nickserv >= 1) $
		mapM_ (\(chan, pass) -> Join chan >>> pass) channels

parseMode (Message Nothing "PING" _) = Raw >>> "PONG :Ayekarambaa"

parseMode _ = return ()

modifyKey :: (Ord k) => k -> k -> Map k a -> Map k a
modifyKey old new m = case M.lookup old m of
	Nothing	-> m
	Just a	-> M.insert new a . M.delete old $ m


findprefix :: [String] -> String -> Maybe String
findprefix	[]	_	= Nothing
findprefix	(x:xs)	input	= case shavePrefixWith toLower x input of
					Nothing -> findprefix xs input
					a	-> a

getAccess :: Sender -> StateT River IO Access
getAccess who = do
	Config {access}	<- gets config

	return $ case find (\(_, n) -> n == who) access of
		Nothing -> Peon
		Just (a, _) -> a
