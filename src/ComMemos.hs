module ComMemos (list) where
import Helpers

import CommandInterface
import Config
import Memos

list :: CommandList
list = [
	("memo"		, (comMemo	, 2	, Peon	, "<receiver> <<message>>"
		, "Store a memo which will be sent once any indication of life is found from the receiver."))
	]


comMemo :: Command
comMemo nick args Info{echo} ComState{conn} = do
	q	<- saveMemos conn to nick mess
	echo $ if q
		then nick ++ ", Memo to " ++ to ++ " saved."
		else nick ++ ", Database unavailable. (This shouldn't happen)"
	where (to, mess) = breakDrop isSpace args
