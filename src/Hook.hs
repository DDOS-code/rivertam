module Hook (sender, sendExternal) where
import Control.Strategies.DeepSeq
import qualified Data.Map as M

import Helpers
import IRC
import Send
import Parse
import Memos
import Command
import CommandInterface
import Config

sender :: SenderChan -> Response -> IO ()
sender tchan = atomically . writeTChan tchan . strict . responseToIrc

sendExternal :: ComState -> IrcState -> Config -> SenderChan -> FilePath -> External -> IO ()
sendExternal state irc config tchan confDir (ExecCommand access chan person string) = command info state access person string
	where
	info = Info {
		  echo		= sender tchan . Msg chan
		, echop		= sender tchan . (if chan =|= person then Msg else Notice) person
		, filePath	= confDir
		, config
		, myNick	= ircNick irc
		, userList	= maybe M.empty (M.map (const ())) $ M.lookup (map toLower chan) (ircMap irc)
		}


sendExternal ComState{conn} _ _ tchan _ (BecomeActive person) =
	mapM_ (echo . show) =<< fetchMemos conn person
	where echo = sender tchan . Msg person
