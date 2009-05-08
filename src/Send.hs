module Send (
	  module Control.Concurrent
	, module Control.Monad.STM
	, module Control.Concurrent.STM.TChan
	, clearSender
	, senderThread
) where
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Helpers

--Delay in microseconds for the spam-protection
mdelay :: Integer
mdelay = 2500000


senderThread :: Handle -> TChan String -> IO ()
senderThread sock buffer = printer 0 =<< getMicroTime where
	printer buf tlast = do
		tnow		<- getMicroTime
		let nbuf	= max 0 (buf-(tnow-tlast))
		if nbuf <= mdelay*4
			then do
				string <- atomically $ readTChan buffer
				hPutStrLn sock string
				putStrLn $ "\x1B[31;1m<<\x1B[30;0m " ++ show string
				printer (nbuf+mdelay) tnow
			else do
				let delay = nbuf-(mdelay*4)
				threadDelay $ fromIntegral delay
				now <- getMicroTime
				printer (buf-(now-tnow)) now


clearSender :: TChan a -> STM ()
clearSender buffer = do
	isempty <- isEmptyTChan buffer
	unless isempty $ do
		readTChan buffer
		clearSender buffer
