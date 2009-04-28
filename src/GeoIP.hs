module GeoIP (
	  Data
	, Value
	, fromFile
	, getCountry
) where
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Array hiding ((//))
import Data.Word

import Helpers

type Data = Array Word Value
type Value = (Word32, B.ByteString)

fromFile :: FilePath -> IO Data
fromFile file = (toArray . getCVS . B.lines) `liftM` B.readFile file

getCVS :: [B.ByteString] -> [Value]
getCVS []				= []
getCVS (x:xs)
	| B.null x || B.head x == '#'	= getCVS xs
	| otherwise			= (toTuple . B.split ',') x : getCVS xs
	where	toTuple (a:_:_:_:_:_:c:[])	= (r a, fix c)
		toTuple _			= error "Errors in the ip-to-countries file."
		fix	= B.init . B.tail
		r	= fromIntegral .fst . fromJust . B.readInt . fix

getCountry :: Data -> Word32 -> String
getCountry arr ip
	| ip >= vstart && ip <= vend	= loop (aend // 2) (aend//4)
	| otherwise 			= "Unknown"

	where
	(astart, aend)	= bounds arr
	(vstart, vend)	= (fst $ arr!astart, fst $ arr!aend)
	loop n add
		| ip >= a2	= loop (n + add) add'
		| ip < a1	= loop (n - add) add'
		| otherwise	= capitalize . B.unpack $ b
		where	(a1, b) = arr!n
			(a2, _) = arr!(n+1)
			add'	= if add > 1 then add//2 else 1

toArray :: (Num b, Ix b) => [a] -> Array b a
toArray x = listArray (0, fromIntegral $ length x-1) x
