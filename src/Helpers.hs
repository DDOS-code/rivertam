{-# LANGUAGE CPP #-}
module Helpers (
	module Data.Char
	, DNSEntry(..), Nocase(..)
	, split, splitlines, breakDrop
	, firstWord, stripw
	, fromNull, maybeL
	, replace, dropWhileRev
	, (//), (%), atLeastLen
	, intmean, mread, lookupDelete
	, getMicroTime, getUnixTime
	, stripContainer, stripPrefixWith, stripSuffix, stripInfix
	, capitalize, formatTime, getIP
	, liftMS, getDNS
) where
import Data.Char
import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Control.Monad hiding (forM_, mapM_, msum, sequence_)
import System.Time
import Data.Foldable
import Data.List (stripPrefix)
import Text.Read
import Control.Strategies.DeepSeq
import Control.Applicative
import Network.Socket
#ifndef linux_HOST_OS
import Network.BSD
import Data.Word
#endif

data DNSEntry = DNSEntry {dnsFamily :: !Family, dnsAddress :: !SockAddr} deriving Show

newtype Nocase = Nocase {recase :: String}

instance Eq Nocase where
	Nocase a == Nocase b = (fmap toLower a) == (fmap toLower b)
	Nocase a /= Nocase b = (fmap toLower a) /= (fmap toLower b)

instance Ord Nocase where
	Nocase a `compare` Nocase b = (fmap toLower a) `compare` (fmap toLower b)

instance DeepSeq Nocase where
	deepSeq (Nocase x) = deepSeq x

instance Read Nocase where
	readPrec = do
		String x <- lexP
		return $ Nocase x

instance Show Nocase where
	show = show . recase


stripw :: String -> String
stripw = dropWhileRev isSpace . dropWhile isSpace

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split func s = case dropWhile func s of
	[] -> []
	s' -> w : split func s''
		where (w, s'') = break func s'

breakDrop :: (a -> Bool) -> [a] -> ([a], [a])
breakDrop f xs = let (a, b) = break f xs
	in (a, dropWhile f b)

splitlines :: String -> [String]
splitlines = split (\a -> a == '\n' || a == '\r')

firstWord :: String -> String
firstWord = takeWhile (not . isSpace)

atLeastLen :: Integral t => t -> [a] -> Bool
atLeastLen 0 _		= True
atLeastLen _ []		= False
atLeastLen n (_:xs)	= atLeastLen (n-1) xs

fromNull :: [a] -> [a] -> [a]
fromNull x []	= x
fromNull _ x	= x

maybeL :: f -> (a -> f) -> [a] -> f
maybeL x _ []		= x
maybeL _ f (xs:_)	= f xs

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace s r = rep where
	rep []		= []
	rep xx@(x:xs)	= case stripPrefix s xx of
				Nothing	-> x : rep xs
				Just a	-> r ++ rep a

(//), (%) :: Integral a => a -> a -> a
(//) = div
(%) = mod

intmean :: (Integral i, Foldable f) => f i -> i
intmean l = if len == 0 then 0 else lsum // len where
	(len, lsum) = foldl' (\(!n, !s) elm -> (n+1, s+elm)) (0, 0) l

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []


mread :: (Read a) => String -> Maybe a
mread x = case reads x of
	[(a, "")]	-> Just a
	_		-> Nothing

lookupDelete :: (Eq k) => k -> [(k, v)] -> (Maybe v, [(k, v)])
lookupDelete key = roll where
	roll []			= (Nothing, [])
	roll (x@(a, b):xs)
		| key == a	= (Just b, xs)
		| otherwise	= let ~(may, xs') = roll xs in (may, x:xs')

stripPrefixWith :: (Eq t) => (t -> t) -> [t] -> [t] -> Maybe [t]
stripPrefixWith	_	[]	y	= Just y
stripPrefixWith	_	_	[]	= Nothing
stripPrefixWith	f	(x:xs)	(y:ys)	= if f x == f y then stripPrefixWith f xs ys else Nothing

stripContainer :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
stripContainer x y l = stripPrefix x l >>= stripSuffix y

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix _	[]	= Nothing
stripSuffix p	xss@(x:xs)
	| p == xss	= Just []
	| otherwise	= (x:) `liftM` stripSuffix p xs

stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix _	[]	= Nothing
stripInfix p	xss@(x:xs) = case stripPrefix p xss of
	Nothing	-> x `f` stripInfix p xs
	Just a	-> Just ([], a)
	where	f a (Just (as, bs))	= Just (a:as, bs)
		f _ Nothing		= Nothing


capitalize :: String -> String
capitalize = unwords . map f . words
	where	f []		= []
		f (x:xs)	= toUpper x : fmap toLower xs


formatTime :: (Integral i) => i -> String
formatTime s = f day "day" ++ ", " ++ f hour "hour" ++ ", " ++ f min' "minute" ++  " and " ++ f sec "second"
	where
	sec	= s % 60
	min'	= (s // 60) % 60
	hour	= (s // (60*60)) % 24
	day	= (s // (60*60*24))

	f val str	= show val ++ ' ':str ++ (if val == 1 then "" else "s")


getIP :: String -> (String, String)
getIP str = case break (==':') str of
		(a, [])	-> (a, "30720")
		(a, b)	-> (a, drop 1 b)

liftMS :: (Monad m) => (a -> b) -> (m a) -> (m b)
liftMS f v = (\x -> return $! f x) =<< v

--Some IO stuff
getMicroTime, getUnixTime :: IO Integer
getMicroTime = let f (TOD s p) = s*1000000 + p//1000000 in f <$> getClockTime
getUnixTime = let f (TOD s _) = s in f <$> getClockTime


getDNS :: String -> String -> IO DNSEntry

#ifdef linux_HOST_OS
getDNS host_ port_ = do
	AddrInfo _ family _ _ addr _ <- head `liftM` getAddrInfo Nothing (Just host_) (Just port_)
	return $ DNSEntry family addr

#else
getDNS host_ port_ = do
	HostEntry _ _ family addr <- getHostByName host_
	let port = read port_ :: Word16
	return $ DNSEntry family (SockAddrInet (fromIntegral port) (head addr))
#endif
