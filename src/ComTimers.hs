module ComTimers(list) where
import System.Time
import System.Locale
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.IORef
import Control.Applicative

import CommandInterface

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

comUptime _ _ Info{echo} ComState{uptime} = echo =<< format uptime <$> getUnixTime
	where
	format started now = "Running for " ++ formatTime sec ++ ". e-shoes: " ++ eshoes
		where
		sec	= fromInteger (now-started) ::Int
		day	= sec // 86400 + 1 -- She must have at least 1 pair of shoes, thus the +1 :)
		eshoes	= "|" ++ (replicate day '.') ++ "|"


comCountdownAdd nick mess Info{echo} ComState{counter, countdownS=tvar} = do
	case mread mess :: Maybe Countdown of
		Nothing	-> echo $ "\STXcountdown:\STX Syntax error."
		Just a -> do
			n <- readIORef counter
			countdown n tvar (show nick) echo a
			modifyIORef counter (+1)

comCountdown _ mess Info{echo} ComState{countdownS=tvar}  = do
	thmap	<- atomically $ readTVar tvar
	case mread $ firstWord mess of
		Nothing	-> do
			echo $ case [show x ++ ":" ++ show comment ++ "("++y++")" | (x, (y, Countdown _ comment _, _)) <- M.toList thmap] of
				[]	-> "No active countdowns."
				a	-> "\STXCurrent countdowns:\STX " ++ intercalate " \STX|\STX " a
		Just a	-> case M.lookup a thmap of
			Nothing	-> echo $ "\STXcountdownkill:\STX Invalid ID"
			Just (name, Countdown finish comment _, _) -> do
				let finishstr	= formatCalendarTime defaultTimeLocale "%c" . toUTCTime . (\x -> TOD x 0)
				echo $ "Countdown \""++comment++"\", created by " ++ name ++ ". Will finish at "
					++ finishstr finish ++ "."


comCountdownKill _ mess Info{echo} ComState{countdownS=tvar} =
	case mread $ firstWord mess of
		Nothing -> echo $ "\STXcountdownkill:\STX Invalid argument. (expecting integer)"
		Just a -> do
			thmap	<- atomically $ readTVar $ tvar
			case M.lookup a thmap of
				Nothing	 -> echo $ "\STXcountdownkill:\STX Invalid ID"
				Just (_,_,tid) -> do
					killThread tid
					atomically $ modifyTVar tvar $ M.delete a
					echo $ "\STXcountdownkill:\STX Countdown id " ++ show a ++ " killed."


countdown :: Int -> CountdownType -> String -> (String -> IO ()) -> Countdown -> IO ()
countdown n tvar nick f c@(Countdown time comment final) = do
	m	<- atomically $ newEmptyTMVar
	tid	<- forkIO $ atomically (takeTMVar m) >> loop
	atomically $ modifyTVar tvar $ M.insert n (nick, c, tid)
	atomically $ putTMVar m ()
	where
	loop = do
		TOD now _	<- getClockTime
		if now >= time then do
			f $ "\STX==>\STX " ++ comment ++ ": " ++ final ++ " \STX<=="
			atomically $ modifyTVar tvar $ M.delete n
		 else do -- ugly spce
			let	diff 		= time-now
				untilnext	= min diff (max 15 (diff // 4))
			f $ comment ++ ": " ++ formatTime diff ++ "."
			bigThreadDelay . (*1000000) $ untilnext

			loop

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar t f = readTVar t >>= \x -> writeTVar t (f x)

formatTime :: (Integral i) => i -> String
formatTime s = f day "day" ++ ", " ++ f hour "hour" ++ ", " ++ f min' "minute" ++  " and " ++ f sec "second"
	where
	sec	= s % 60
	min'	= (s // 60) % 60
	hour	= (s // (60*60)) % 24
	day	= (s // (60*60*24))

	f val str	= show val ++ ' ':str ++ (if val == 1 then "" else "s")


bigThreadDelay :: Integer -> IO ()
bigThreadDelay t
	| t > intMax	= threadDelay maxBound >> bigThreadDelay (t - intMax)
	| otherwise	= threadDelay (fromIntegral t)
	where intMax = fromIntegral (maxBound :: Int)

