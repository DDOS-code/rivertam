module ComTremRelay where
import Send
import TremLib
import System.IO
import Control.Monad
import Helpers

tremToIrc :: TChan String -> String -> FilePath -> IO ()
tremToIrc tchan ircchan fifo = do
	-- The fifo has to be opened in ReadWrite Mode to prevent it from reaching EOF right away.
	-- Nothing will ever be written to it however.
	hdl <- openFile fifo ReadWriteMode
	hSetBuffering hdl NoBuffering
	forever $ do
		tremline <- (removeColors . filter isPrint) `liftM` hGetLine hdl
		case breakList ": irc: " =<< shaveFirstPrefix ["] say: ", " say: "] tremline of
			Just (name, mess) ->
				atomically $ writeTChan tchan $
				"PRIVMSG " ++ ircchan ++ " :<[T] "++name++"> " ++ mess

			_ -> return ()


--Some functions to extract the info
shaveFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe [a]
shaveFirstPrefix	(p:ps)	lst	=
	 case shavePrefix p lst of
		Nothing -> shaveFirstPrefix ps lst
		a	-> a
shaveFirstPrefix	[]	_	= Nothing

breakList :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakList at lst = splitX =<< infixAt at lst
	where splitX pos = Just $ (a, b)
		where
		(a, tmp)	= splitAt pos lst
		b 		= drop (length at) tmp

infixAt :: (Num n, Eq a) => [a] -> [a] -> Maybe n
infixAt inf = loop 0 where
	loop n		xx@(_:xs)	= if isPrefixOf inf xx then Just n else loop (n+1) xs
	loop _		[]		= Nothing

