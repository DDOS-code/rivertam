import ClanStat
import Text.Printf
import Text.Html


gameToHTML (date, clan, map, (s1, s2)) =
	printf	"<tr class=\"%s\">\n\
		\\t<td>%s</td>\n\
		\\t<td>%s</td>\n\
		\\t<td>%s</td>\n\
		\\t<td>%s</td>\n\
		\</tr>\n"
		(stringToHtmlString $ wldclass totscore)
		(stringToHtmlString date)
		(stringToHtmlString clan)
		(stringToHtmlString map)
		(stringToHtmlString score)
		:: String
	where	totscore = s1 +| s2
		score = (\(a,b,c) -> printf "%d:%d:%d" a b c :: String) totscore
		wldclass (a,b,c) = if a > b then "won" else if a < b then "lost" else "draw"



main = do
	cont <- getClanFile
	putStr . concat . map gameToHTML . reverse $ cont
