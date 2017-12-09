{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger               (MonadLogger)
import           Control.Monad.Logger
import           Control.Monad.Reader               (ask)
import           Control.Monad.Reader
import           Data.Aeson                         hiding (json)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as L
import           Data.Time.Calendar                 (Day (ModifiedJulianDay),
                                                     toModifiedJulianDay)
import           Data.Time.Clock
import           Database.Esqueleto
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Lib                                (QueJob)
import           System.Log.FastLogger
import           Web.Scotty
import           Web.Scotty.Internal.Types          (ActionEnv, ActionT,
                                                     getParams)

main :: IO ()
main = do
  conn <- connectPostgreSQL ""
  (logger, _) <- newFastLogger $ LogStdout defaultBufSize
  backend :: SqlBackend <- openSimpleConn (nullLogger logger) conn
  jobs <- runReaderT getJobs backend
  mapM_ (print . toEncoding . entityVal) jobs
    where nullLogger logger _ _ _ = logger

getJobs :: MonadIO m => SqlReadT m [Entity QueJob]
getJobs = select $ from $ (\p -> return p)

data Job = Job
  { jobPriority   :: Int
  , jobRunAt      :: UTCTime
  , jobId         :: Int
  , jobClass      :: T.Text
  , jobArgs       :: T.Text
  , jobErrorCount :: Int
  , jobLastError  :: T.Text
  , jobQueue      :: T.Text
  , jobRetryable  :: Bool
  , jobFailedAt   :: UTCTime
  }

instance ToJSON Job where
  toJSON j = object ["id" .= jobId j]

instance FromRow Job where
  fromRow =
    Job <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*>
    field <*>
    field <*>
    field


summary :: ActionM ()
summary = liftIO getQueueSummary >>= json

getQueueSummary :: IO [Summary]
getQueueSummary = do
  conn <- connectPostgreSQL ""
  query_
    conn
    "SELECT job_class, priority, count(*) as count, \
                     \sum((s.job_id is not null)::int) as count_working \
              \FROM que_jobs \
              \LEFT JOIN ( \
              \   SELECT job_id \
              \   FROM que_jobs \
              \   JOIN ( \
              \     SELECT ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) as pg_lock_id \
              \     FROM pg_locks \
              \     WHERE pg_locks.locktype = 'advisory' \
              \     AND pg_locks.objsubid = 1 \
              \   ) l on l.pg_lock_id = que_jobs.job_id \
              \) s using (job_id) \
              \WHERE queue = '' \
              \AND (run_at <= now() AND retryable = true) \
              \GROUP BY job_class, priority \
              \ORDER BY priority, lower(job_class)"

data Summary = Summary
  { summaryJobClass     :: T.Text
  , summaryPriority     :: Int
  , summaryCount        :: Int
  , summaryCountWorking :: Int
  }

instance ToJSON Summary where
  toJSON s =
    object
      [ "job_class" .= summaryJobClass s
      , "priority" .= summaryPriority s
      , "count" .= summaryCount s
      , "count_working" .= summaryCountWorking s
      ]

instance FromRow Summary where
  fromRow = Summary <$> field <*> field <*> field <*> field
