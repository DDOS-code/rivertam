{-# LANGUAGE CPP #-}
module Helpers (
	module Data.Char
	, DNSEntry(..)
	, Caseless(..)
	, stripw
	, split
	, breakDrop
	, splitlines
	, takeAll
	, firstWord
	, atLeastLen
	, replace
	, (=|=), (=/=), (//), (%)
	, intmean
	, dropWhileRev
	, mread
	, getMicroTime
	, shaveOfContainer
	, shavePrefix
	, shaveSuffix
	, shavePrefixWith
	, shaveInfix
	, capitalize
	, getIP
	, liftMS
	, readFileStrict
	, whenJust
	, getDNS
) where
import Data.Char
import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Control.Monad hiding (forM_, mapM_, msum, sequence_)
import System.Time
import Data.Foldable

import qualified Data.ByteString.Char8 as B
import Network.Socket

#ifndef linux_HOST_OS
import Network.BSD
import Data.Word
#endif

data DNSEntry = DNSEntry {dnsFamily :: !Family, dnsAddress :: !SockAddr} deriving Show

newtype Caseless = Caseless String

instance Eq Caseless where
	Caseless a == Caseless b = (fmap toLower a) == (fmap toLower b)
	Caseless a /= Caseless b = (fmap toLower a) /= (fmap toLower b)

instance Ord Caseless where
	Caseless a `compare` Caseless b = (fmap toLower a) `compare` (fmap toLower b)

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

takeAll :: (a -> Bool) -> [a] -> [a]
takeAll	_	[]	= []
takeAll	cmp	(x:xs)
	| cmp x		= x:takeAll cmp xs
	| otherwise	= takeAll cmp xs

firstWord :: String -> String
firstWord = takeWhile (not . isSpace)

atLeastLen :: Num t => t -> [a] -> Bool
atLeastLen 0 _		= True
atLeastLen _ []		= False
atLeastLen n (_:xs)	= atLeastLen (n-1) xs

replace :: Eq a => ([a], [a]) -> [a] -> [a]
replace (s,r) = rep where
	rep []		= []
	rep xx@(x:xs)	= case shavePrefix s xx of
				Nothing	-> x : rep xs
				Just a	-> r ++ rep a

(=|=), (=/=) :: (Functor f, Eq (f Char)) => f Char -> f Char -> Bool
a =|= b = (fmap toLower a) == (fmap toLower b)
a =/= b = (fmap toLower a) /= (fmap toLower b)

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

shavePrefixWith :: (Eq t) => (t -> t) -> [t] -> [t] -> Maybe [t]
shavePrefixWith	_	[]	y	= Just y
shavePrefixWith	_	_	[]	= Nothing
shavePrefixWith	f	(x:xs)	(y:ys)	= if f x == f y then shavePrefix xs ys else Nothing

shaveOfContainer :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
shaveOfContainer x y l = shavePrefix x l >>= shaveSuffix y

shavePrefix, shaveSuffix :: Eq a => [a] -> [a] -> Maybe [a]

shavePrefix = shavePrefixWith id

shaveSuffix _	[]	= Nothing
shaveSuffix p	xss@(x:xs)
	| p == xss	= Just []
	| otherwise	= (x:) `liftM` shaveSuffix p xs

shaveInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
shaveInfix _	[]	= Nothing
shaveInfix p	xss@(x:xs) = case shavePrefix p xss of
	Nothing	-> x `f` shaveInfix p xs
	Just a	-> Just ([], a)
	where	f a (Just (as, bs))	= Just (a:as, bs)
		f _ Nothing		= Nothing


capitalize :: String -> String
capitalize = unwords . map f . words
	where	f []		= []
		f (x:xs)	= toUpper x : fmap toLower xs

getIP :: String -> (String, String)
getIP str = case break (==':') str of
		(a, [])	-> (a, "30720")
		(a, b)	-> (a, drop 1 b)

liftMS :: (Monad m) => (a -> b) -> (m a) -> (m b)
liftMS f v = (\x -> return $! f x) =<< v

--Some IO stuff
getMicroTime :: IO Integer
getMicroTime = f `fmap` getClockTime
	where f (TOD s p) = s*1000000 + p//1000000

readFileStrict :: FilePath -> IO String
readFileStrict file = B.unpack `liftM` B.readFile file

whenJust :: (Monad m) => Maybe a -> (a -> m()) -> m ()
whenJust Nothing	_	= return ()
whenJust (Just a)	f	= f a


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
