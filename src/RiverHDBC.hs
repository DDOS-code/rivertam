module RiverHDBC where
import River
import Database.HDBC
import Database.HDBC.PostgreSQL

sqlArg0 :: (MonadState RState m, MonadIO m) => (Connection -> IO a) -> m a
sqlArg0 f = gets conn >>= \c -> io $ f c

sqlArg2 :: (MonadState RState m, MonadIO m) => (Connection -> a1 -> a2 -> IO a) -> a1 -> a2 -> m a
sqlArg2 f a b = gets conn >>= \conn -> io $ f conn a b

sqlQuery', sqlQuery:: (MonadState RState m, MonadIO m) => String -> [SqlValue] -> m [[SqlValue]]
sqlQuery' = sqlArg2 quickQuery'
sqlQuery = sqlArg2 quickQuery

sqlRun :: (MonadState RState m, MonadIO m) => String -> [SqlValue] -> m Integer
sqlRun = sqlArg2 run
