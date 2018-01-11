{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sql where

import           Data.Aeson
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (isJust)
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
  , summaryRunAt        :: UTCTime
  , summaryCount        :: Int
  , summaryCountWorking :: Int
  }
instance FromRow SummaryRow where
  fromRow = SummaryRow <$> field <*> field <*> field <*> field <*> field
instance ToJSON SummaryRow where
  toJSON r = object [
      "job_class" .= summaryClass
    , "priority" .= summaryPriority
    , "run_at" .= summaryRunAt
    , "count" .= summaryCount
    , "count_working" .= summaryCountWorking
    ]
      where SummaryRow{..} = r
queueSummary :: Connection -> Text -> IO [SummaryRow]
queueSummary conn queue = query conn [sql|
    SELECT
      job_class,
      priority,
      min(run_at) as run_at,
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
  , jobRunAt              :: UTCTime
  , jobId                 :: Int
  , jobClass              :: Text
  , jobArgs               :: Value
  , jobErrorCount         :: Int
  , jobLastError          :: Maybe Text
  , jobQueue              :: Text
  , jobRetryable          :: Bool
  , jobFailedAt           :: Maybe UTCTime
  , jobFailed             :: Bool
  , jobScheduledForFuture :: Bool
  }
instance FromRow JobRow where
  fromRow = JobRow <$> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field <*> field
instance ToJSON JobRow where
  toJSON j = object [
      "priority" .= jobPriority
    , "run_at" .= jobRunAt
    , "job_id" .= jobId
    , "job_class" .= jobClass
    , "args" .= jobArgs
    , "error_count" .= jobErrorCount
    , "last_error" .= jobLastError
    , "queue" .= jobQueue
    , "failed_at" .= jobFailedAt
    , "failed" .= jobFailed
    , "retryable" .= jobRetryable
    , "scheduled_for_future" .= jobScheduledForFuture
    ]
      where JobRow{..} = j

-- Get all jobs matching the filter
jobs :: Connection -> JobFilter -> IO [JobRow]
jobs conn f = query conn [sql|
  SELECT
    priority,
    run_at,
    job_id,
    job_class,
    args,
    error_count,
    last_error,
    queue,
    retryable,
    failed_at,
    (error_count > 0)::bool AS failed,
    run_at > now() AS scheduled_for_future
  FROM
    que_jobs
  LEFT JOIN (
    SELECT
      ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) AS pg_lock_id
    FROM
      pg_locks
    WHERE pg_locks.locktype = 'advisory'
    AND pg_locks.objsubid = 1
  ) l
  ON l.pg_lock_id = que_jobs.job_id
  WHERE ((? = true AND priority = ?) OR (? = false))
  AND   ((? = true AND job_class = ?) OR (? = false))
  AND   ((? = true AND queue = ?) OR (? = false))
  AND   ((? = true AND (error_count > 0) = ? AND l.pg_lock_id IS NULL) OR (? = false))
  ORDER BY retryable::int DESC,
           (run_at < now())::int DESC,
           priority ASC, run_at ASC
  LIMIT 100
  |] f
-- The filter for the jobs query.
-- If a field is empty, it will match jobs with any value.
data JobFilter = JobFilter
  { filterPriority :: Maybe Int
  , filterClass    :: Maybe Text
  , filterQueue    :: Maybe Text
  , filterFailed   :: Maybe Bool
  }
instance ToRow JobFilter where
  toRow JobFilter {
    filterPriority = p
  , filterClass = c
  , filterQueue = q
  , filterFailed = f
  }  = [toField byP, toField p, toField byP,
        toField byC, toField c, toField byC,
        toField byQ, toField q, toField byQ,
        toField byF, toField f, toField byF]
    where (byP, byC, byQ, byF) = (isJust p, isJust c, isJust q, isJust f)

-- A single job
job :: Connection -> Int -> IO (Maybe JobRow)
job conn jobId = do
  js <- query conn [sql|
    SELECT
      priority,
      run_at,
      job_id,
      job_class,
      args,
      error_count,
      last_error,
      queue,
      retryable,
      failed_at,
      (error_count > 0)::bool AS failed,
      run_at > now() AS scheduled_for_future
    FROM que_jobs
    WHERE job_id = ?
    LIMIT 1
    |] [jobId]
  case js of
    [j] -> return (Just j)
    _   -> return Nothing

-- Retry a single job
retryJob :: Connection -> Int -> IO (Maybe JobRow)
retryJob conn jobId = do
  js <- query conn [sql|
    UPDATE
      que_jobs
    SET
      retryable = true
    WHERE
      job_id = ?
    RETURNING *
    |] [jobId]
  case js of
    [j] -> return (Just j)
    _   -> return Nothing

data FailureRow = FailureRow {
    failureClass        :: Text
  , failureInactive     :: Int
  , failurePendingRetry :: Int
  }
instance FromRow FailureRow where
  fromRow = FailureRow <$> field <*> field <*> field
instance ToJSON FailureRow where
  toJSON r = object [
      "job_class" .= failureClass
    , "inactive" .= failureInactive
    , "pending_retry" .= failurePendingRetry
    ]
      where FailureRow{..} = r
-- A summary of all failed jobs
failureSummary :: Connection -> IO [FailureRow]
failureSummary conn = query_ conn [sql|
    SELECT
      job_class,
      sum((NOT retryable)::int) AS inactive,
      sum((error_count > 0 AND retryable)::int) AS pending_retry
    FROM que_jobs
    LEFT JOIN (
      SELECT
        ((pg_locks.classid::bigint << 32) | pg_locks.objid::bigint) AS pg_lock_id
      FROM
        pg_locks
      WHERE pg_locks.locktype = 'advisory'
      AND pg_locks.objsubid = 1
    ) l ON l.pg_lock_id = que_jobs.job_id
    WHERE l.pg_lock_id IS NULL
    AND ((NOT retryable) OR (error_count > 0 AND retryable))
    GROUP BY
      job_class
    ORDER BY
      sum(error_count) DESC
  |]
