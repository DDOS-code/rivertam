module Send (
	  module Control.Concurrent
	, module Control.Monad.STM
	, module Control.Concurrent.STM.TChan
	, Send(..)
	, SendNew(..)
	, (>>>)
	, clearSender
	, senderThread
) where
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import RiverState
import Helpers

data Send = Raw | Msg !String | Me !String | Join !String | Part !String | Notice !String | Nick | Kick !String
data SendNew = NMsg String | NNotice String

--Delay in microseconds for the spam-protection
mdelay :: Integer
mdelay = 2500000

infixr 0 >>>
(>>>) :: Send -> String -> RiverState
(>>>) method !arg = do
	sender <- gets rivSender
	let mess = case method of
		Raw		-> arg
		Msg	chan 	-> "PRIVMSG "++chan++" :"++arg
		Me	chan	-> "PRIVMSG "++chan++" :\1ACTION "++arg++"\1"
		Notice	to	-> "NOTICE "++to++" :"++arg
		Join	chan	-> "JOIN "++chan++" "++arg
		Part	chan	-> "PART "++chan++" :"++arg
		Nick		-> "NICK "++arg
		Kick	chan	-> "KICK "++chan++" "++arg
	lift $ atomically $ writeTChan sender mess


senderThread :: Handle -> TChan String -> IO ()
senderThread sock buffer = printer 0 =<< getMicroTime where
	printer buf tlast = do
		tnow <- getMicroTime
		let	diff = tnow-tlast
			nbuf = buf-diff
			nbuf' = if nbuf < 0 then 0 else nbuf
		if nbuf' <= mdelay*4 then do
			string <- atomically $ readTChan buffer
			hPutStrLn sock string
			putStrLn $ "\x1B[31;1m<<\x1B[30;0m " ++ show string
			printer (nbuf'+mdelay) tnow
		 else do
		 	let delay = nbuf'-(mdelay*4)
			threadDelay $ fromIntegral delay
			now <- getMicroTime
			printer (buf-(now-tnow)) now


clearSender :: TChan a -> IO ()
clearSender buffer = atomically loop where
	loop = do
		isempty <- isEmptyTChan buffer
		unless isempty $ do
			readTChan buffer
			loop
