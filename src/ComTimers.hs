module ComTimers(list) where
import Control.Applicative
import CommandInterface

list :: CommandList
list =	[("uptime"		, (comUptime		, 0	, Peon		, ""
		, "Displays uptime (obviously)."))
	]

comUptime :: Command
comUptime _ _ Info{echo} ComState{uptime} = echo =<< format uptime <$> getUnixTime
	where
	format started now = "Running for " ++ formatTime sec ++ ". e-shoes: " ++ eshoes
		where
		sec	= fromInteger (now-started) ::Int
		day	= sec // 86400 + 1 -- She must have at least 1 pair of shoes, thus the +1 :)
		eshoes	= "|" ++ (replicate day '.') ++ "|"

formatTime :: (Integral i) => i -> String
formatTime s = f day "day" ++ ", " ++ f hour "hour" ++ ", " ++ f min' "minute" ++  " and " ++ f sec "second"
	where
	sec	= s % 60
	min'	= (s // 60) % 60
	hour	= (s // (60*60)) % 24
	day	= (s // (60*60*24))

	f val str	= show val ++ ' ':str ++ (if val == 1 then "" else "s")
