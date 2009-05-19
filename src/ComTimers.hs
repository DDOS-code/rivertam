module ComTimers(list) where
import System.Time
import System.Locale
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M

import CommandInterface
import Config
import Helpers



list :: CommandList
list =	[
	  ("uptime"		, (comUptime		, 0	, Peon		, ""
		, "Displays uptime (obviously)."))
	, ("countdown-add"	, (comCountdownAdd	, 3	, User		, "<target-unix-time> <\"message\"> <\"finished-string\">"
		, "Create a countdown event."))
	, ("countdown-kill"	, (comCountdownKill	, 1	, Master	, "<id>"
		, "Kills a countdown."))
	, ("countdown"		, (comCountdown		, 0	, Peon		, "(id)"
		, "Shows existing countdowns, or, with a supplied argument, displays info about that specific coutdown event."))
	]


comUptime, comCountdown, comCountdownAdd, comCountdownKill :: Command

comUptime _ _ _ = do
	started		<- gets uptime
	TOD now _	<- lift getClockTime
	let	sec	= fromInteger (now-started) :: Int
		day	= sec // 86400
		eshoes	= "|" ++ replicate ((day+1)*2) '.' ++ "|"
		str	= "Running for "++formatTime sec++". e-shoes: " ++ eshoes
	echo . Mess $ str


comCountdownAdd nick_ mess _ = do
	echoFunc	<- gets echoFunc
	counter		<- gets counter
	tvar		<- gets countdownS
	modify $ \x -> x {counter=counter+1}

	case mread mess :: Maybe Countdown of
		Nothing	-> echo . Mess $ "\STXcountdown:\STX Syntax error."
		Just a -> lift $ countdown counter tvar nick (echoFunc . Mess) a
	where nick = map toLower nick_

comCountdown _ mess _ = do
	tvar		<- gets countdownS
	thmap		<- lift $ atomically $ readTVar tvar
	case arg of
		Nothing	-> do
			echo . Mess $ case [show x ++ ":" ++ show comment ++ "("++y++")" | (x, (y, Countdown _ comment _, _)) <- M.toList thmap] of
				[]	-> "No active countdowns."
				a	-> "\STXCurrent countdowns:\STX " ++ intercalate " \STX|\STX " a
		Just a	-> case M.lookup a thmap of
			Nothing	-> echo . Mess $ "\STXcountdownkill:\STX Invalid ID"
			Just (name, Countdown finish comment _, _) -> do
				let finishstr	= formatCalendarTime defaultTimeLocale "%c" . toUTCTime . (\x -> TOD x 0)
				echo . Mess $
					"Countdown \""++comment++"\", created by " ++ name ++ ". Will finish at "
					++ finishstr finish ++ "."
	where	arg	= mread $ takeWhile (not . isSpace) mess :: Maybe Int


comCountdownKill _ mess _
	| isNothing args	=
		echo . Mess $ "\STXcountdownkill:\STX Invalid argument. (expecting integer)"
	| otherwise		= do
		tvar		<- gets countdownS
		thmap		<- (lift . atomically . readTVar) tvar
		case M.lookup a thmap of
			Nothing	 -> echo . Mess $ "\STXcountdownkill:\STX Invalid ID"
			Just (_,_,tid) -> do
				lift $ killThread tid
				lift $ atomically $ writeTVar tvar (M.delete a thmap)
				echo . Mess $ "\STXcountdownkill:\STX Countdown id " ++ show a ++ " killed."
	where	args	= mread $ head $ words mess :: Maybe Int
		a	= fromJust args



countdown :: Int -> CountdownType -> String -> (String -> IO ()) -> Countdown -> IO ()
countdown n tvar nick f c@(Countdown time comment final) = do
	tid	<- forkIO $ (threadDelay 300 >> loop)
	current	<- atomically $ readTVar tvar
	atomically $ writeTVar tvar (M.insert n (nick, c, tid) current)

	where
	loop = do
		TOD now _	<- getClockTime
		if now >= time then do
			f $ "\STX==>\STX " ++ comment ++ ": " ++ final ++ " \STX<=="
			atomically $ do
				tmp	<- readTVar tvar
				writeTVar tvar (M.delete n tmp)
		 else do -- ugly spce
			let	diff 		= time-now
				untilnext	= min 15 (diff // 4)
			f $ comment ++ ": " ++ formatTime diff ++ "."
			bigThreadDelay . (*1000000) $ untilnext
			loop

formatTime :: (Integral i) => i -> String
formatTime sec_ = pDay ++ ", " ++ pHour ++ ", " ++ pMin ++  " and " ++ pSec
	where
	pDay			= test day "day"
	pHour			= test hour "hour"
	pMin			= test minute "minute"
	pSec			= test sec "second"

	(minute_, hour_, day)	= (sec_//60, minute_//60, hour_//24)
	(sec, minute, hour)	= (sec_%60, minute_%60, hour_%24)

	test val str	= show val ++ ' ':str ++ (if val == 1 then "" else "s")


bigThreadDelay :: Integer -> IO ()
bigThreadDelay t
	| t > fromIntegral intMax	= threadDelay maxBound >> bigThreadDelay (t - intMax)
	| otherwise			= threadDelay (fromIntegral t)
	where intMax = fromIntegral (maxBound :: Int)

