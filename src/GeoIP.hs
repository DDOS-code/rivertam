module GeoIP (
	  Data(..)
	, fromFile
	, getCountry
) where
import Control.Exception (bracket)
import System.IO
import System.IO.Unsafe
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Array.Unboxed hiding ((//))
import Data.Word
import qualified Data.Map as M

import Helpers

type IPIndex	= Word
type IP		= Word32
type Lookup	= Word16
type Country	= String

data Data = Data !(UArray IPIndex IP)		-- Ip-array (Index | IP-range start)
		 !(UArray IPIndex Lookup)	-- Number to lookup array (Index | Country Index)
		 !(Array Lookup Country)	-- Number to country array (Country Index | Country String)

lazyLines :: Handle -> IO [B.ByteString]
lazyLines fx = do
	eof <- hIsEOF fx
	if eof then return [] else unsafeInterleaveIO $ liftM2 (:) (B.hGetLine fx) (lazyLines fx)


fromFile :: FilePath -> IO Data
fromFile file = bracket (openFile file ReadMode) hClose $ \x ->
	getCVS =^! lazyLines x



--A lot of bang-spam :)
getCVS :: [B.ByteString] -> Data
getCVS = getCVSnew [] [] M.empty 0 0 where
	getCVSnew !buf !bufnum !cmap !cmapnum !len (!x:xs)
		| B.null x || B.head x == '#'	= getCVSnew buf bufnum cmap cmapnum len xs
		| otherwise			= getCVSnew (ip:buf) (cnum:bufnum) cmap' cmapnum' (len+1) xs
		where	(!ip, !country) = extractLine x
			(!cnum, !cmap', !cmapnum') = case M.lookup country cmap of
				Just a	-> (a, cmap, cmapnum)
				Nothing	-> (cmapnum, M.insert country cmapnum cmap, cmapnum+1)
	getCVSnew !buf !bufnum !cmap !cmapnum !len [] = Data iparray indexarray indexedmap where
		iparray		= listArray (0, len-1) $ reverse buf
		indexarray	= listArray (0, len-1) $ reverse bufnum
		indexedmap	= array (0, cmapnum-1) $ map (\(!a,!b)-> (b, capitalize . B.unpack $ a)) (M.toList cmap)


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
