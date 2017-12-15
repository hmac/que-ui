{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sql where

import qualified Data.Map.Strict                  as Map
import           Data.Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ (sql)

-- Hits the database to check that we still have a connection,
-- then returns {"db": "ok", "api": "ok}
healthCheck :: Connection -> IO (Map.Map Text Text)
healthCheck conn = do
  [Only s :: Only Text] <- query_ conn "select 'ok'::text"
  return $ Map.insert "db" s (Map.singleton "api" "ok")

-- A summary of all active workers, showing what jobs they are working
-- and how long they've been working them.
workers :: Connection -> IO [(Text, Text, Text, Text, UTCTime, Text)]
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
queueSummary :: Connection -> Text -> IO [(Text, Text, Integer, Integer)]
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
