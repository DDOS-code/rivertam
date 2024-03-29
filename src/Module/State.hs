module Module.State where
import Network.Tremulous.Protocol
import Database.HDBC.PostgreSQL
import Network.Socket (Socket)
import Control.Concurrent (ThreadId)
import Config

data HolderTrem = HolderTrem !Integer ![MasterServer] ![GameServer]
data TremRelay = TremRelay !(Maybe Socket) !(Maybe ThreadId)

data State = State
	{ trempoll	:: !HolderTrem
	, conn		:: !Connection
	, relay		:: !TremRelay
	}

start :: Config -> IO State
start config = do
	conn <- connectPostgreSQL (pgconn config)
	return State	{ trempoll	= HolderTrem 0 [] []
			, relay		= TremRelay Nothing Nothing
			, .. }
