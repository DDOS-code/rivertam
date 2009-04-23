module TremLib (
	  tremulousFindSimple
	, tremulousStats
	, tremulousFindServer
	, tremulousClanList
	, ircifyColors
	, removeColors
) where
import qualified Data.Map as M
import Data.Array hiding ((//))
import Helpers
import TremMasterCache
import Network.Socket


tremulousFindSimple :: ServerCache -> String -> [(String, [String])]
tremulousFindSimple (polled, _) searchstring = echo where
	echo 		= filter (\(_, b) -> not $ null b) onlyplayers
	onlyplayers 	= [(name (show ip) cvars, [x | (_,_,_,x)<-ps, find' (playerGet x)]) | (ip, (cvars, ps)) <- M.toList polled]
	find'		= isInfixOf (map toLower searchstring)
	name a b	= maybe a (stripw . take 50 . filter isPrint) (M.lookup "sv_hostname" b)

tremulousFindServer :: ServerCache -> String -> Maybe (SockAddr, ServerInfo)
tremulousFindServer (polled, _) searchstring = echo mp where
	echo	[]	= Nothing
	echo	x	= Just $ snd $ minimum x
	mp		= [(a, b) | (Just a, b) <- [(findName (fst info),(ip, info)) | (ip, info) <-M.toList polled]]
	findName x	= case M.lookup "sv_hostname" x of
		Nothing	-> Nothing
		Just a	-> if isInfixOf (map toLower searchstring) (playerGet a) then Just $ length a else Nothing


tremulousClanList :: ServerCache -> [String] -> [(Int, String)]
tremulousClanList (polled, _) clanlist = sortfunc fplayers
	where
	sortfunc	= sortBy (\a b -> compare b a) . filter (\(a,_) -> a > 1)
	fplayers	= map (\a -> (length $ filter (isInfixOf (map toLower a)) players, a)) clanlist
	players		= map (\(_,_,_,a) -> playerGet a) . concat $ [p | (_,(_, p)) <-M.toList polled]

tremulousStats :: ServerCache -> (Int, Int, Int)
tremulousStats (polled, unresponsive) = (a, b, players) where
	(a, b)	= (M.size polled, a + unresponsive)
	players	= length [() | (_,_,p,_) <- concat [ps | (_, (_, ps)) <- M.toList polled], p /= 0]


playerGet, removeColors, ircifyColors :: String -> String

playerGet = removeColors . map toLower

removeColors []					= []
removeColors (c:[])				= [c]
removeColors (c:cs@(_:css))
	| c == '^'	= removeColors css
	| otherwise	= c : removeColors cs

ircifyColors []					= []
ircifyColors (c:[])				= c:"\SI"
ircifyColors (c:cs@(cs1:css))
	| c == '^' && cs1 >= '0' && cs1 <= '9'	= mc!cs1 ++ ircifyColors css
	| otherwise				= c : ircifyColors cs
	where mc = listArray ('0', '9') ["\SI", "\ETX04", "\ETX09", "\ETX08", "\ETX12", "\ETX11", "\ETX13", "\SI", "\SI", "\ETX04"]
