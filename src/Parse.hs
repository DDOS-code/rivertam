module Parse (parseIrcLine) where
import qualified Data.Map as M
import System.Random

import ComTremRelay
import Config
import Send
import Helpers
import RiverState
import Commands


parseIrcLine :: String -> RiverState
parseIrcLine (':':line)	= parseMode $ splitcolon line
parseIrcLine line	= parseRMode $ words line

--Ircmode command parser
parseMode, parseRMode :: [String] -> RiverState

parseMode (s0:"PRIVMSG":reciever:mess:_) = do
	rivNick				<- gets rivNick
	Config {comkey, alias}		<- gets config

	let	nuh@(sender,_,_)	= nicksplit s0
		cPrefixes		= [comkey, rivNick++", ", rivNick++": "]
	gotaccess			<- getAccess nuh
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

parseMode (s0:"KICK":s2:s3:_) = do
	Config {channels}		<- gets config
	rivNick				<- gets rivNick
	let	(chan, kickedPerson) 	= (s2, s3)
		(sender,_,_)		= nicksplit s0
		pass			= fromMaybe [] $ lookup chan channels

	rivMap <- gets rivMap
	modify $ \x -> x {rivMap=M.adjust (M.delete (map toLower kickedPerson)) (map toLower chan) rivMap}

	when (rivNick == kickedPerson) $ do
		Join chan >>> pass
		Msg chan >>> sender++", thanks very much for the kick!"

--":Cadynum-Pirate!n=cadynum@unaffiliated/cadynum NOTICE river-tam|pirate :test"
parseMode (s0:"NOTICE":s2:s3:[]) = do
	rivNick		<- gets rivNick
	let nuh		= nicksplit s0
	gotaccess	<- getAccess nuh
	when (rivNick =|= s2 &&  gotaccess == Master) $ do
		Raw >>> s3

-- User list
--":kornbluth.freenode.net 353 river-tam = ##ddos :river-tam @stoned_es Spartakusafk @raf_kig @Cadynum @Saliva Fleurka @PhilH @ChanServ"
parseMode (_:"353":_:_:a:b:[]) = do
	let	chan	= map toLower a
		users	= p353toTuples b

	rivMap <- gets rivMap
	let	newchanmap = M.singleton chan (M.fromList users)
		woo = M.unionWith M.union newchanmap rivMap
	modify $ \x -> x {rivMap=woo}

parseMode (a:"JOIN":c:_) = do
	let	chan		= map toLower c
		(nick,_,_)	= nicksplit $ map toLower a
	rivMap <- gets rivMap
	let foo = M.insertWith M.union chan (M.singleton nick Normal) rivMap
	modify $ \x -> x {rivMap=foo}

parseMode (a:"QUIT":_) = do
	rivMap	<- gets rivMap
	let	(nick,_,_)	= nicksplit $ map toLower a
		nmap		= M.map (M.delete nick) rivMap
	modify $ \x -> x {rivMap=nmap}

--":Cadynum!n=cadynum@unaffiliated/cadynum PART ##ddos :\"Moo!\""
parseMode (a:"PART":c:_) = do
	let	chan		= map toLower c
		(nick,_,_)	= nicksplit $ map toLower a
	rivMap <- gets rivMap
	let foo = M.adjust (M.delete nick) chan rivMap
	modify $ \x -> x {rivMap=foo}

--":JoKe|!i=joke@lyseo.edu.ouka.fi NICK :JoKe|hungry"
parseMode (a:"NICK":c:_) = do
	rivMap			<- gets rivMap
	rivNick			<- gets rivNick
	let	(nick,_,_)	= nicksplit . map toLower $  a
		newnick		= map toLower c
		nmap		= M.map (modifyKey nick newnick) rivMap
	modify $ \x -> x {rivMap=nmap}

	when (nick =|= rivNick) $ do
		modify $ \x -> x {rivNick=newnick}

--":kornbluth.freenode.net 001 river-tam|30 :Welcome to the freenode IRC Network river-tam|30"
parseMode (_:"001":mynick:_) = do
	Config {nickserv} <- gets config
	rivNick			<- gets rivNick
	when (not $ null nickserv) $
		Msg "NickServ" >>> "IDENTIFY "++nickserv

	when (rivNick =|= initNick_) $ do
		modify $ \x -> x {rivNick=mynick}


-- >> ":grisham.freenode.net 433 * staxie :Nickname is already in use."
parseMode (_:"433":_:_:_) = do
	Config {nick}		<- gets config
	rivNick			<- gets rivNick

	unless (nick =|= rivNick) $ do
		rand		<- lift $ getStdRandom . randomR $ (0, 100::Int)
		let newnick	= (take 10 nick) ++ show rand
		Nick >>> newnick


--Nickserv signed in.
parseMode (_:"901":_) = do
	Config {nickserv, channels} <- gets config
	when (length  nickserv >= 1) $
		mapM_ (\(chan, pass) -> Join chan >>> pass) channels

--Or end of motd
parseMode (_:"376":_) = do
	Config {nickserv, channels} <- gets config
	unless (length  nickserv >= 1) $
		mapM_ (\(chan, pass) -> Join chan >>> pass) channels

parseMode _ = return ()

parseRMode ("PING":_) = Raw >>> "PONG :Ayekarambaa"

parseRMode _ = return ()

modifyKey :: (Ord k) => k -> k -> Map k a -> Map k a
modifyKey old new m = case M.lookup old m of
	Nothing	-> m
	Just a	-> M.insert new a . M.delete old $ m

p353toTuples :: String -> [(String, Status)]
p353toTuples s =  map match (words $ map toLower s)  where
	match ('@':n)	= (n, OP)
	match ('+':n)	= (n, Voice)
	match n		= (n, Normal)

splitcolon :: String -> [String]
splitcolon xs = let (a, b) = break (==':') xs in words a ++ [drop 1 b]

findprefix :: [String] -> String -> Maybe String
findprefix	[]	_	= Nothing
findprefix	(x:xs)	input	= case shavePrefixWith toLower x input of
					Nothing -> findprefix xs input
					a	-> a

getAccess :: NUH -> StateT River IO Access
getAccess who = do
	Config {access}	<- gets config

	return $ case find (\(_, n) -> matchnuh n who) access of
		Nothing -> Peon
		Just (a, _) -> a


matchnuh :: NUH -> NUH -> Bool
matchnuh (a1, a2, a3) (b1, b2, b3) = mi a1 b1 && mi a2 b2 && mi a3 b3
	where	mi []	_	= True
		mi x	y	= map toLower x == map toLower y
