{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sql where

import           Data.Aeson
import qualified Data.Map.Strict                    as Map
import           Data.Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

-- Hits the database to check that we still have a connection,
-- then returns {"db": "ok", "api": "ok}
healthCheck :: Connection -> IO (Map.Map Text Text)
healthCheck conn = do
  [Only s :: Only Text] <- query_ conn "select 'ok'::text"
  return $ Map.insert "db" s (Map.singleton "api" "ok")

-- A summary of all active workers, showing what jobs they are working
-- and how long they've been working them.
data WorkerRow = WorkerRow {
    workerClass          :: Text
  , workerId             :: Int
  , workerQueue          :: Text
  , workerPid            :: Int
  , workerStartedAt      :: UTCTime
  , workerProcessingTime :: Text
  }
instance FromRow WorkerRow where
  fromRow = WorkerRow <$> field <*> field <*> field
                      <*> field <*> field <*> field
instance ToJSON WorkerRow where
  toJSON r = object [
      "job_class" .= workerClass
    , "job_id" .= workerId
    , "queue" .= workerQueue
    , "pid" .= workerPid
    , "started_at" .= workerStartedAt
    , "processing_time" .= workerProcessingTime
    ]
      where WorkerRow{..} = r
workers :: Connection -> IO [WorkerRow]
workers conn = query_ conn [sql|
    SELECT
      que_jobs.job_class AS job_class,
      que_jobs.job_id AS job_id,
      que_jobs.queue AS queue,
      pg.pid AS pid,
      pg.query_start AS started_at,
      to_char(now() - pg.query_start, 'HH24:MI:SS') AS processing_time
    FROM
      que_jobs
    JOIN (
      SELECT ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) AS job_id,
             pg_stat_activity.*
      FROM pg_locks
      JOIN pg_stat_activity USING (pid)
      WHERE locktype = 'advisory'
    ) pg USING (job_id)
    ORDER BY pid
  |]

-- A summary of all jobs currently being worked, grouped by job class.
data SummaryRow = SummaryRow {
    summaryClass        :: Text
  , summaryPriority     :: Int
  , summaryCount        :: Int
  , summaryCountWorking :: Int
  }
instance FromRow SummaryRow where
  fromRow = SummaryRow <$> field <*> field <*> field <*> field
instance ToJSON SummaryRow where
  toJSON r = object [
      "job_class" .= summaryClass
    , "priority" .= summaryPriority
    , "count" .= summaryCount
    , "count_working" .= summaryCountWorking
    ]
      where SummaryRow{..} = r
queueSummary :: Connection -> Text -> IO [SummaryRow]
queueSummary conn queue = query conn [sql|
    SELECT
      job_class,
      priority,
      count(*) AS count,
      sum((s.job_id IS NOT NULL)::int) AS count_working
    FROM
      que_jobs
    LEFT JOIN (
      SELECT
        job_id
      FROM que_jobs
      JOIN (
        SELECT
          ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) AS pg_lock_id
        FROM
          pg_locks
        WHERE pg_locks.locktype = 'advisory'
        AND pg_locks.objsubid = 1
      ) l ON l.pg_lock_id = que_jobs.job_id
    ) s USING (job_id)
    WHERE
      queue = ?::text
      AND (run_at <= now() AND retryable = true)
    GROUP BY
      job_class, priority
    ORDER BY
      priority, lower(job_class)
  |] [queue]

data JobRow = JobRow {
    jobPriority           :: Int
  , jobId                 :: Int
  , jobClass              :: Text
  , jobArgs               :: Value
  , jobFailed             :: Bool
  , jobErrorCount         :: Int
  , jobQueue              :: Text
  , jobRetryable          :: Bool
  , jobScheduledForFuture :: Bool
  }
instance FromRow JobRow where
  fromRow = JobRow <$> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field <*> field
instance ToJSON JobRow where
  toJSON j = object [
      "priority" .= jobPriority
    , "job_id" .= jobId
    , "job_class" .= jobClass
    , "args" .= jobArgs
    , "failed" .= jobFailed
    , "error_count" .= jobErrorCount
    , "queue" .= jobQueue
    , "retryable" .= jobRetryable
    , "scheduled_for_future" .= jobScheduledForFuture
    ]
      where JobRow{..} = j

-- There's now ToRow instance for a 12 element tuple, so we declare one here
-- TODO: remove this and use a dedicated type instead
instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l]

jobs :: Connection -> (Bool, Maybe Int, Bool, Bool, Maybe Text, Bool, Bool, Maybe Text, Bool, Bool, Maybe Bool, Bool) -> IO [JobRow]
jobs conn filters = query conn [sql|
  SELECT
    priority,
    job_id,
    job_class,
    args,
    (error_count > 0)::bool AS failed,
    error_count,
    queue,
    retryable,
    run_at > now() AS scheduled_for_future
  FROM
    que_jobs
  WHERE ((? = true AND priority = ?) OR (? = false))
  AND   ((? = true AND job_class = ?) OR (? = false))
  AND   ((? = true AND queue = ?) OR (? = false))
  AND   ((? = true AND (error_count > 0) = ?) OR (? = false))
  ORDER BY retryable::int DESC,
           (run_at < now())::int DESC,
           priority ASC, run_at ASC
  LIMIT 100
  |] filters

failedJobs :: Connection -> IO [JobRow]
failedJobs conn = query_ conn [sql|
    SELECT
      priority,
      que_jobs.job_id,
      job_class,
      args,
      (error_count > 0)::bool AS failed,
      error_count,
      queue,
      retryable,
      run_at > now() AS scheduled_for_future
    FROM
      que_jobs
    LEFT JOIN (
      SELECT
        job_id
      FROM que_jobs
      JOIN (
        SELECT
          ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) AS pg_lock_id
        FROM
          pg_locks
        WHERE pg_locks.locktype = 'advisory'
        AND pg_locks.objsubid = 1
      ) l ON l.pg_lock_id = que_jobs.job_id
    ) s ON s.job_id = que_jobs.job_id AND s.job_id IS NULL
  |]
