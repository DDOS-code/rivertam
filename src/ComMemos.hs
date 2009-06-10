module ComMemos (list) where
import Data.IORef
import System.Time

import CommandInterface
import Config
import Helpers
import Memos

list :: CommandList
list = [
	("memo"		, (comMemo	, 2	, Peon	, "<to> <message>"
		, "Store a memo which will be sent once any indication of life is found from the receiver."))
	]


comMemo :: Command
comMemo nick args Info{echo} ComState{memos} = do
	time	<- getClockTime
	modifyIORef memos $ saveMemo to (Entry time nick mess)
	echo $ nick ++ ", Memo saved."
	where (to, drop 1 -> mess) = break isSpace args
