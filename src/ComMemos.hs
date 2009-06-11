module ComMemos (list) where
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
	saveMemos memos to nick mess
	echo $ nick ++ ", Memo to " ++ to ++ " saved."
	where (to, dropWhile isSpace -> mess) = break isSpace args
