module ComMemos (list) where
import CommandInterface
import Memos

list :: CommandList
list = [("memo"		, (comMemo	, 2	, Peon	, "<receiver> <<message>>"
		, "Store a memo which will be sent once any indication of life is found from the receiver."))]

comMemo :: Command
comMemo nuh@(Name (Nocase nick) _ _) args Info{echo} ComState{conn} = do
	saveMemos conn to (show nuh) mess
	echo $ nick ++ ", Memo to " ++ to ++ " saved."
	where (to, mess) = breakDrop isSpace args
