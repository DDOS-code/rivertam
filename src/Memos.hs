module Memos where
import Data.Map(Map)
import qualified Data.Map as M
import Data.Sequence
import qualified Data.Foldable as F
import Data.Char

import System.Time
import System.Locale

data Entry = Entry ClockTime String String

instance Show Entry where
	show (Entry time from mess) = date ++ " - Message from " ++ from ++ ": " ++ mess
		where date = formatCalendarTime defaultTimeLocale "%c" (toUTCTime time)

type Memos = Map String (Seq Entry)

saveMemo :: String -> Entry -> Memos -> Memos
saveMemo key memo = M.insertWith' (flip (><)) (map toLower key) (singleton memo)

fetchMemo :: String -> Memos -> [Entry]
fetchMemo key = maybe [] F.toList . M.lookup (map toLower key)

removeMemo :: String -> Memos -> Memos
removeMemo key = M.delete (map toLower key)
