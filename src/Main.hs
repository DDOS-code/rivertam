{-
	Rivertam, written by Christoffer Öjeling aka "Cadynum".

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU Affero General Public License as
	published by the Free Software Foundation, either version 3 of the License,
	or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU Affero General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Main where
import Rivertam
import Irc.Protocol
import Control.Concurrent
import Data.List
import Control.Monad

import Module.Core
import Module.Essential
import Module.Tremulous
import Module.State
import Module.Quotes
import Module.Clans
import Module.ClanWar
import Module.Memos
import Module.Alias


main :: IO ()
main = rivertam Hooks
	{ comHook	= Module.State.start
	, moduleHook	= [ Module.Core.mdl, Module.Essential.mdl, Module.Tremulous.mdl, Module.Quotes.mdl
			  , Module.Clans.mdl, Module.ClanWar.mdl, Module.Memos.mdl, Module.Alias.mdl]
	, initHook	= return ()
	, quitHook	= myQuitHook
	, eventHook	= myEventHook
	, aliasHook	= Module.Alias.alias
	}

myQuitHook = do
	uptime <- (-) <$> io getUnixTime <*> gets initTime
	send $ Quit $ "rivertam - The haskell IRC-bot with style! - Running " ++ formatTime uptime
	io $ threadDelay 1000000 --Give it one second to send the Quit message

myEventHook (Message x y) = case (y, x) of
	(Msg c msg, NUH (Name nick _ _)) -> do
		when ("xD" `isInfixOf` msg) $
			send $ Msg c $ "\SI" ++ recase nick ++ ", \ETX12xD\SI = e\ETX12X\SItreme retar\ETX12D\SI?"
		sendM =<< map (Msg nick . show) <$> (fetchMemos nick)
	(Join _ _, NUH (Name nick _ _)) ->
		sendM =<< map (Msg nick . show) <$> (fetchMemos nick)
	_ -> return ()
