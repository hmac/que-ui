{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Data.Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Text.RawString.QQ

hello :: Connection -> IO Int
hello conn = do
  [Only i] <- query_ conn "select 2 + 2"
  return i

main :: IO ()
main = do
  conn <- connectPostgreSQL ""
  h <- hello conn
  putStrLn $ "hello: " ++ show h
  jobs conn >>= print
  workers conn >>= print
  return ()

jobs :: Connection -> IO Int
jobs conn = do
  [Only c] <- query_ conn "select count(*) from que_jobs"
  return c

workers :: Connection -> IO [(Text, Text, Text, Text, UTCTime, Text)]
workers conn = query_ conn [r|
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
