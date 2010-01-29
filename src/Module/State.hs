module Module.State where
import Tremulous.Polling
import Database.HDBC.PostgreSQL
import Config

data HolderTrem = HolderTrem !Integer [MasterServer] !PollResponse

data State = State
	{ trempoll	:: !HolderTrem
	, conn		:: !Connection
	}

start :: Config -> IO State
start config = do
	conn <- connectPostgreSQL (pgconn config)
	return State{ trempoll= HolderTrem 0 [] emptyPoll, .. }
