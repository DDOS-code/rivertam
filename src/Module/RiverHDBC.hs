module Module.RiverHDBC (
	  module Database.HDBC
	, sqlArg0, sqlArg2
	, sqlRun, sqlQuery, sqlQuery', sqlTransaction, sqlTransactionTry, sqlIfNotTable
) where
import Module
import Module.State
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL


sqlArg0 :: (MonadState (RState State) m, MonadIO m) => (Connection -> IO a) -> m a
sqlArg0 f = getc >>= \c -> io $ f c

sqlArg2 :: (MonadState (RState State) m, MonadIO m) => (Connection -> a1 -> a2 -> IO a) -> a1 -> a2 -> m a
sqlArg2 f a b = getc >>= \c -> io $ f c a b

sqlQuery', sqlQuery :: (MonadState (RState State) m, MonadIO m) => String -> [SqlValue] -> m [[SqlValue]]
sqlQuery' = sqlArg2 quickQuery'
sqlQuery = sqlArg2 quickQuery

sqlRun :: (MonadState (RState State) m, MonadIO m) => String -> [SqlValue] -> m Integer
sqlRun = sqlArg2 run

getc :: (MonadState (RState State) m) => m Connection
getc = gets (conn . com)

-- Sql functions -----------------------------------------------------------------------------------

sqlTransactionTry :: RiverCom State a -> (SqlError -> RiverCom State a) -> RiverCom State a
sqlTransactionTry f errf = catchRC toTry toFail
	where
	toTry = do
		x <- f
		sqlArg0 commit
		return x
	toFail e = do
		ignoreException (sqlArg0 rollback)
		x <- errf e
		return x
	ignoreException x = catchRC x $ \(_ :: SqlError) -> return ()


sqlTransaction :: RiverCom State a -> RiverCom State a
sqlTransaction f = catchRC toTry toFail where
	toTry = do x <- f; sqlArg0 commit; return x
	toFail :: SqlError -> RiverCom State a
	toFail e = sqlArg0 rollback >> throw e

sqlIfNotTable :: (MonadState (RState State) m, MonadIO m) => String -> [String] -> m ()
sqlIfNotTable tbl x = do
	tables <- sqlArg0 getTables
	unless (tbl `elem` tables) $ mapM_ (\a -> sqlRun a [] >> sqlArg0 commit) x
