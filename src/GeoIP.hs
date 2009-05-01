module GeoIP (
	  Data(..)
	, fromFile
	, getCountry
) where
import System.IO
import System.IO.Unsafe
import Control.Monad
import Control.Exception (bracket)
import Control.Parallel.Strategies
import qualified Data.ByteString.Char8 as B
import Data.Array.Diff hiding ((//))
import qualified Data.Array.Diff as A ((//))
import qualified Data.Map as M
import Data.Word

import Helpers


type IPIndex	= Word
type IP		= Word32
type Lookup	= Word16
type Country	= String

data Data = Data !(DiffUArray IPIndex IP)		-- Ip-array (Index | IP-range start)
		 !(DiffUArray IPIndex Lookup)	-- Number to lookup array (Index | Country Index)
		 !(Array Lookup Country)	-- Number to country array (Country Index | Country String)

lazyLines :: Handle -> IO [B.ByteString]
lazyLines fx = do
	eof <- hIsEOF fx
	if eof then return [] else unsafeInterleaveIO $ liftM2 (:) (B.hGetLine fx) (lazyLines fx)


fromFile :: FilePath -> IO Data
fromFile file = bracket (openFile file ReadMode) hClose $ \x -> do
	lc	<- countLines =^! lazyLines x
	hSeek x AbsoluteSeek 0
	getCVS lc =^! lazyLines x


countLines :: (Integral a) => [B.ByteString] -> a
countLines = foldl' trv 0 where
	trv !n x	| B.null x || B.head x == '#'	= n
			| otherwise			= n+1

getCVS :: Word -> [B.ByteString] -> Data
getCVS lc = loop (array (0,lc-1) []) (array (0,lc-1) []) M.empty 0 0 where
	loop !ipA !clA !cmap !cmapnum !n (x:xs)
		| B.null x || B.head x == '#'	= loop ipA clA cmap cmapnum n xs
		| otherwise			= loop ipA' clA' cmap' cmapnum' (n+1) xs
		where	ipA'		= ipA A.// [(n, ip)]
			clA'		= clA A.// [(n, cnum)]
			(!ip, !country) = extractLine x
			(!cnum, !cmap', !cmapnum') = case M.lookup country cmap of
				Just a	-> (a, cmap, cmapnum)
				Nothing	-> (cmapnum, M.insert country cmapnum cmap, cmapnum+1)
	loop !ipA !clA !cmap !cmapnum _ [] = Data ipA clA indexedmap where
		indexedmap	= forceval seqArr $ array (0, cmapnum-1) $ map (\(!a,!b)-> (b, capitalize . B.unpack $ a)) (M.toList cmap)


extractLine :: (Integral i) => B.ByteString -> (i, B.ByteString)
extractLine = toTuple . B.split ',' where
	toTuple [a,_,_,_,_,_,c]	= (r a, fix c)
	toTuple _		= error "Error in the ip-to-countries file."
	fix	= B.init . B.tail
	r	= fromIntegral . fst . fromJust . B.readInt . fix

getCountry :: Data -> Word32 -> String
getCountry (Data arr indexarr ltbl) !ip
	| ip >= vstart && ip <= vend	= loop (aend // 2) (aend//4)
	| otherwise 			= "Unknown"

	where
	(astart, aend)	= bounds arr
	(vstart, vend)	= (arr!astart, arr!aend)
	loop !n !add
		| ip >= a2	= loop (n + add) add'
		| ip < a1	= loop (n - add) add'
		| otherwise	= ltbl!(indexarr!n)
		where	a1		= arr!n
			a2		= arr!(n+1)
			add'		= if add > 1 then add//2 else 1
