module GeoIP (
	  Data(..)
	, fromFile
	, getCountry
) where
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Array.Unboxed hiding ((//))
import Data.Word
import qualified Data.Map as M

import Helpers

data Data = Data !(UArray Word Word32)		-- Ip-array
		 !(UArray Word Word)		-- Number to lookup array
		 !(Array Word B.ByteString)	-- Number to country array

fromFile :: FilePath -> IO Data
fromFile file = (getCVS . B.lines) `liftM` B.readFile file

getCVS :: [B.ByteString] -> Data
getCVS = getCVSnew [] [] M.empty 0 0 where
	getCVSnew !buf !bufnum !cmap !cmapnum !len (!x:xs)
		| B.null x || B.head x == '#'	= getCVSnew buf bufnum cmap cmapnum len xs
		| otherwise			= getCVSnew (ip:buf) (cnum:bufnum) cmap' cmapnum' (len+1) xs
		where	(!ip, !country) = toTuple . B.split ',' $ x
			(!cnum, !cmap', !cmapnum') = case M.lookup country cmap of
				Just a	-> (a, cmap, cmapnum)
				Nothing	-> (cmapnum, M.insert country cmapnum cmap, cmapnum+1)
			toTuple !(a:_:_:_:_:_:c:[])	= (r a, fix c)
			toTuple _			= error "Errors in the ip-to-countries file."
			!fix	= B.init . B.tail
			!r	= fromIntegral .fst . fromJust . B.readInt . fix
	getCVSnew !buf !bufnum !cmap !cmapnum !len [] = Data iparray indexarray indexedmap where
		iparray	= listArray (0, len-1) $ reverse buf
		indexarray	= listArray (0, len-1) $ reverse bufnum
		indexedmap	= array (0, cmapnum-1) $ map (\(!a,!b)-> (b,a)) (M.toList cmap)


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
		| otherwise	= capitalize . B.unpack $ ltbl!(indexarr!n)
		where	a1		= arr!n
			a2		= arr!(n+1)
			add'		= if add > 1 then add//2 else 1

{-
toArray :: (Num b, Ix b) => [a] -> Array b a
toArray x = listArray (0, fromIntegral $ length x-1) x
-}
