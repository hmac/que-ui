{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (MonadLogger)
import           Control.Monad.Logger
import           Control.Monad.Reader                 (ask)
import           Control.Monad.Reader
import           Data.Int                             (Int64)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L
import           Data.Time.Calendar                   (Day (ModifiedJulianDay),
                                                       toModifiedJulianDay)
import           Data.Time.Clock
import           Database.Esqueleto                   as E
import           Database.Esqueleto.Internal.Language (Value)
import           Database.Esqueleto.Internal.Sql      (SqlExpr, unsafeSqlBinOp)
import           Database.Persist
import qualified Database.Persist.Postgresql          as P
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Lib                                  (EntityField (..), Lock,
                                                       Oid, QueJob)
import           System.Log.FastLogger
import           Web.Scotty                           as S
import           Web.Scotty.Internal.Types            (ActionEnv, ActionT,
                                                       getParams)

main :: IO ()
main = do
  conn <- connectPostgreSQL ""
  (logger, _) <- newFastLogger $ LogStdout defaultBufSize
  backend :: SqlBackend <- P.openSimpleConn (nullLogger logger) conn
  runServer backend
    where nullLogger logger _ _ _ = logger

runServer :: SqlBackend -> IO ()
runServer backend = S.scotty 3001 $ do
  S.get "/jobs" $ do
    jobs <- liftIO $ runReaderT getJobs backend
    json jobs
  S.get "/locks/advisory" $ do
    locks <- liftIO $ runReaderT getAdvisoryLocks backend
    json locks
  S.get "/locks" $ do
    locks <- liftIO $ runReaderT getLocks backend
    json locks

getJobs :: MonadIO m => SqlReadT m [Entity QueJob]
getJobs = select $ from $ (\p -> return p)

getLocks :: MonadIO m => SqlReadT m [Entity Lock]
getLocks = select $ from $ (\p -> return p)

getAdvisoryLocks :: MonadIO m => SqlReadT m [Entity Lock]
getAdvisoryLocks = select $ from $ \l -> do
  where_ (l ^. LockLocktype E.==. val "advisory")
  where_ (l ^. LockObjsubid E.==. just (val 1))
  where_ ((((l ^. LockObjsubid) <<. Oid 32) |. (l ^. LockObjid)) E.==. (k ^. QueJobId))
  return l

-- getLockedJobs = select $ from $ \(j, l) -> do
-- 	where_ (j ^. QueJobId E.==. undefined)
-- 	return (j, l)

(<<.) :: (Num a, Num b, Num c) => SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
(<<.) = unsafeSqlBinOp " << "
infix 4 <<.

(|.) :: Num a => SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value a)
(|.) = unsafeSqlBinOp " | "
infixl 6 |.

pgLockId :: expr (Entity Lock) -> SqlExpr (Value Int64)
pgLockId lock = (classid <<. val 32) |. objid
  where classid = lock ^. LockClassid
        objid = lock ^. LockObjid

-- "SELECT job_class, priority, count(*) as count, \
-- 			 \sum((s.job_id is not null)::int) as count_working \
-- \FROM que_jobs \
-- \LEFT JOIN ( \
-- \   SELECT job_id \
-- \   FROM que_jobs \
-- \   JOIN ( \
-- \     SELECT ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) as pg_lock_id \
-- \     FROM pg_locks \
-- \     WHERE pg_locks.locktype = 'advisory' \
-- \     AND pg_locks.objsubid = 1 \
-- \   ) l on l.pg_lock_id = que_jobs.job_id \
-- \) s using (job_id) \
-- \WHERE queue = '' \
-- \AND (run_at <= now() AND retryable = true) \
-- \GROUP BY job_class, priority \
-- \ORDER BY priority, lower(job_class)"
