module Helpers (
	  module Data.Char
	, module Data.List
	, module Data.Maybe
	, NUH
	, stripw
	, split
	, splitlines
	, takeAll
	, replace
	, (=|=), (//), (%)
	, intmean
	, dropWhileRev
	, nicksplit
	, mread
	, getMicroTime
	, shaveOfContainer
	, shavePrefix
	, shaveSuffix
	, shavePrefixWith
	, capitalize
	, getIP
	, readFileStrict
	, whenJust
) where
import Data.Char
import Data.List
import Data.Maybe
import System.Time
import Control.Monad

import qualified Data.ByteString.Char8 as B

type NUH = (String, String, String)

instance (Num a, Num b) => Num ((,) a b) where
	(a1, b1) + (a2, b2) = (a1+a2, b1+b2)
	signum _	= undefined
	_ * _		= undefined
	abs _		= undefined
	fromInteger _	= undefined

instance (Num a, Num b, Num c) => Num ((,,) a b c) where
	(a1, b1, c1) + (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)
	signum _	= undefined
	_ * _		= undefined
	abs _		= undefined
	fromInteger _	= undefined

stripw :: String -> String
stripw = dropWhileRev isSpace . dropWhile isSpace

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split func s = case dropWhile func s of
	[] -> []
	s' -> w : split func s''
		where (w, s'') = break func s'

splitlines :: String -> [String]
splitlines = split (\a -> a == '\n' || a == '\r')

takeAll :: (a -> Bool) -> [a] -> [a]
takeAll	_	[]	= []
takeAll	cmp	(x:xs)
	| cmp x		= x:takeAll cmp xs
	| otherwise	= takeAll cmp xs

replace :: Eq a => ([a], [a]) -> [a] -> [a]
replace (s,r) = rep where
	rep []		= []
	rep xx@(x:xs)	= case shavePrefix s xx of
				Nothing	-> x : rep xs
				Just a	-> r ++ a

(=|=) :: String -> String -> Bool
a =|= b = (map toLower a) == (map toLower b)

(//), (%) :: Integral a => a -> a -> a
(//) = div
(%) = mod

intmean :: (Integral i) => [i] -> i
intmean l = if len == 0 then 0 else lsum // len where
	(len, lsum) = foldl' (\(!n, !s) elm -> (n+1, s+elm)) (0, 0) l

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []


--Split nick!user@host to a 3-tuple
nicksplit :: String -> NUH
nicksplit []	= ([], [], [])
nicksplit str1	= (ifstar a, ifstar b, ifstar c) where
	(a, buf)	= breaksplit '!' str1
	(b, c)		= breaksplit '@' buf
	breaksplit cmp str = (takeWhile (/=cmp) str, (drop 1 . dropWhile (/=cmp)) str)
	ifstar x	= if x == "*" then [] else x

mread :: (Read a) => String -> Maybe a
mread x = case reads x of
	[(a, "")]	-> Just a
	_		-> Nothing

shavePrefixWith :: (Eq t) => (t -> t) -> [t] -> [t] -> Maybe [t]
shavePrefixWith	_	[]	y	= Just y
shavePrefixWith	_	_	[]	= Nothing
shavePrefixWith	f	(x:xs)	(y:ys)	= if f x == f y then shavePrefix xs ys else Nothing

shaveOfContainer :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
shaveOfContainer x y l = shavePrefix x l >>= shaveSuffix y

shavePrefix, shaveSuffix :: Eq a => [a] -> [a] -> Maybe [a]

shavePrefix = shavePrefixWith id

shaveSuffix	x	y
	| isSuffixOf x y	= Just $ take (length y-length x) y
	| otherwise		= Nothing


capitalize :: String -> String
capitalize = unwords . map foo . words
	where	foo []		= []
		foo (x:xs)	= toUpper x : map toLower xs

getIP :: String -> (String, String)
getIP str = case break (==':') str of
		(a, [])	-> (a, "30720")
		(a, b)	-> (a, drop 1 b)

--Some IO stuff
getMicroTime :: IO Integer
getMicroTime = do
	TOD sec pico <- getClockTime
	return $! sec*1000000 + (pico//1000000)

readFileStrict :: FilePath -> IO String
readFileStrict file = B.unpack `liftM` B.readFile file

whenJust :: (Monad m) => Maybe a -> (a -> m()) -> m ()
whenJust Nothing	_	= return ()
whenJust (Just a)	f	= f a
