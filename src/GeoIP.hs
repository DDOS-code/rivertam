module GeoIP (
	  GeoIP(..)
	, fromFile
	, getCountry
) where
import Control.Strategies.DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Array.Diff hiding ((//))
import qualified Data.Array.Diff as A ((//))
import qualified Data.Map as M
import Data.Word
import Data.Maybe
import Control.Applicative

import Helpers

type IPIndex	= Word
type IP		= Word32
type Lookup	= Word16
type Country	= String

data GeoIP = GeoIP !(DiffUArray IPIndex IP)		-- Ip-array (Index | IP-range start)
			!(DiffUArray IPIndex Lookup)	-- Number to lookup array (Index | Country Index)
			!(Array Lookup Country)		-- Number to country array (Country Index | Country String)

lazyLines :: FilePath -> IO [B.ByteString]
lazyLines file = fmap (B.concat . LB.toChunks) . LB.lines <$> LB.readFile file

fromFile :: FilePath -> IO GeoIP
fromFile file = do
	lc <- count <$> lazyLines file
	getCVS lc <$> lazyLines file
	where count = fromIntegral . length . filter  (\x -> not $ B.head x == '#' || B.null x)

getCVS :: Word -> [B.ByteString] -> GeoIP
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
	loop !ipA !clA !cmap !cmapnum _ [] = GeoIP ipA clA indexedmap where
		indexedmap	= strict $ array (0, cmapnum-1) $ map (\(!a,!b)-> (b, capitalize . B.unpack $ a)) (M.toList cmap)


extractLine :: (Integral i) => B.ByteString -> (i, B.ByteString)
extractLine = toTuple . B.split ',' where
	toTuple [a,_,_,_,_,_,c]	= (r a, fix c)
	toTuple _		= err
	fix	= B.init . B.tail
	r	= fromIntegral . fst . fromMaybe err . B.readInt . fix
	err	= error "Error in the ip-to-countries file."

getCountry :: GeoIP -> Word32 -> String
getCountry (GeoIP arr indexarr ltbl) !ip
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
