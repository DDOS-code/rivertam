module Send (
	  module Control.Concurrent
	, module Control.Monad.STM
	, module Control.Concurrent.STM.TChan
	, SenderChan
	, clearSender
	, senderThread
) where
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Helpers

type SenderChan = TChan String

chunkSend :: Integer -> Integer -> IO a -> IO ()
chunkSend delay burst f = loop 0 =<< getMicroTime where
	maxWait = delay * burst
	loop buf'' tlast = do
		tnow	<- getMicroTime
		let buf = max 0 (buf'' - (tnow-tlast))
		if buf <= maxWait
			then f >> loop (buf+delay) tnow
			else do
				let wait = buf - maxWait
				threadDelay $ fromIntegral wait
				loop (buf-delay) (tnow+delay)

senderThread :: Handle -> SenderChan -> IO ()
senderThread sock buffer = chunkSend 2500000 4 $do
	string <- atomically $ readTChan buffer
	hPutStrLn sock string
	putStrLn $ "\x1B[31;1m<<\x1B[30;0m " ++ show string

clearSender :: SenderChan -> STM ()
clearSender buffer = do
	isempty <- isEmptyTChan buffer
	unless isempty $ do
		readTChan buffer
		clearSender buffer
