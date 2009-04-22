module Helpers (
	  module Data.Char
	, module Data.List
	, module Data.Maybe
	, stripw
	, split
	, splitlines
	, takeAll
	, replace
	, (=|=), (//), (%)
	, dropWhileRev
	, nicksplit
	, mread
	, getMicroTime
	, shaveOfContainer
	, shavePrefix
	, shaveSuffix
	, capitalize
) where
import Data.Char
import Data.List
import Data.Maybe
import System.Time
import System.IO.Error (try)

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
replace (s,r) str = rep str where
	len = length s-1
	rep []		= []
	rep xx@(x:xs)	= if isPrefixOf s xx then r ++ (drop len (rep xs)) else x:rep xs

(=|=) :: String -> String -> Bool
a =|= b = (map toLower a) == (map toLower b)

(//), (%) :: Integral a => a -> a -> a
(//) = div
(%) = mod

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []


--Split nick!user@host to a 3-tuple
nicksplit :: String -> (String, String, String)
nicksplit []	= ([], [], [])
nicksplit str1	= (a,b,c) where
	(a, buf)	= breaksplit '!' str1
	(b, c)		= breaksplit '@' buf
	breaksplit cmp str = (takeWhile (/=cmp) str, (drop 1 . dropWhile (/=cmp)) str)

mread :: (Read a) => String -> Maybe a
mread x = case reads x of
	[(a, "")]	-> Just a
	_		-> Nothing

shaveOfContainer :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
shaveOfContainer x y l = shavePrefix x l >>= shaveSuffix y

shavePrefix, shaveSuffix :: Eq a => [a] -> [a] -> Maybe [a]
shavePrefix	[]	y	= Just y
shavePrefix	_	[]	= Nothing
shavePrefix	(x:xs)	(y:ys)	= if x == y then shavePrefix xs ys else Nothing

shaveSuffix	x	y
	| isSuffixOf x y	= Just $ take (length y-length x) y
	| otherwise		= Nothing



capitalize :: String -> String
capitalize = unwords . map foo . words
	where	foo []		= []
		foo (x:xs)	= toUpper x : map toLower xs


--Some IO func's
getMicroTime :: IO Integer
getMicroTime = do
	TOD sec pico <- getClockTime
	return $ sec*1000000 + (pico//1000000)
