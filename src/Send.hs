module Send (
	  module Control.Concurrent
	, SenderChan, clearSender, senderThread
) where
import System.IO
import Control.Concurrent
import Control.Monad

import Helpers

type SenderChan = Chan String

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
senderThread sock buffer = chunkSend 2500000 4 $ do
	string <- readChan buffer
	hPutStrLn sock string
	putStrLn $ "\x1B[31;1m<<\x1B[30;0m " ++ show string

clearSender :: SenderChan -> IO ()
clearSender buffer = do
	isempty <- isEmptyChan buffer
	unless isempty $ do
		readChan buffer
		clearSender buffer
