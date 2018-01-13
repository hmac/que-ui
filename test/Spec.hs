{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception                (bracket, bracket_)
import           Control.Monad
import           Data.Aeson                       (Value (Array))
import           Data.Maybe                       (isJust)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Test.Tasty
import           Test.Tasty.Hspec

import           Sql

main :: IO ()
main = do
  tree <- testSpec "foo" spec
  defaultMain tree

withDb :: (Connection -> IO ()) -> IO ()
withDb = bracket (connectPostgreSQL "" >>= \c -> execute_ c [sql| BEGIN |] >> return c)
                 (flip execute_ $ [sql| ROLLBACK |])

emptyFilter :: JobFilter
emptyFilter =
  JobFilter { filterPriority = Nothing
            , filterClass = Nothing
            , filterQueue = Nothing
            , filterFailed = Nothing
            }

spec :: Spec
spec = do
    describe "jobs" testJobs
    describe "queue summary" testQueueSummary
    describe "job" testJob
    describe "retry job" testRetryJob
    describe "destroy job" testDestroyJob
    describe "retry failures" testRetryFailures
    describe "destroy failures" testDestroyFailures
    describe "failure summary" testFailureSummary
    describe "workers" testWorkers

testJobs :: Spec
testJobs = around withDb $ do
  it "returns a single job when there's only one job" $ \c -> do
    insertJob (defaultArgs { aClass = "foo" }) c
    j <- jobs emptyFilter c
    length j `shouldBe` 1
    jobClass (head j) `shouldBe` "foo"

  it "returns multiple jobs when there are multiple jobs" $ \c -> do
    replicateM_ 2 $ insertJob (defaultArgs { aClass = "foo" }) c
    length <$> jobs emptyFilter c `shouldReturn` 2

  it "filters by job class" $ \c -> do
    let f = emptyFilter { filterClass = Just "foo" }
    insertJob (defaultArgs { aClass = "bar" }) c
    length <$> jobs f c `shouldReturn` 0
    insertJob (defaultArgs { aClass = "foo" }) c
    length <$> jobs f c `shouldReturn` 1

  it "filters by job priority" $ \c -> do
    let f = emptyFilter { filterPriority = Just 100 }
    insertJob (defaultArgs { aPriority = 200 }) c
    length <$> jobs f c `shouldReturn` 0
    insertJob (defaultArgs { aPriority = 100 }) c
    length <$> jobs f c `shouldReturn` 1

  it "filters by queue" $ \c -> do
    let f = emptyFilter { filterQueue = Just "q" }
    insertJob (defaultArgs { aQueue = "" }) c
    length <$> jobs f c `shouldReturn` 0
    insertJob (defaultArgs { aQueue = "q" }) c
    length <$> jobs f c `shouldReturn` 1

  it "filters by failed" $ \c -> do
    let f = emptyFilter { filterFailed = Just True }
    insertJob defaultArgs c
    length <$> jobs f c `shouldReturn` 0
    insertJob (defaultArgs { aErrorCount = 1 }) c
    length <$> jobs f c `shouldReturn` 1

testQueueSummary :: Spec
testQueueSummary = around withDb $ do
  it "returns a summary" $ \c -> do
    insertJob (defaultArgs { aClass = "foo" }) c
    s <- queueSummary c ""
    length s `shouldBe` 1
    summaryClass (head s) `shouldBe` "foo"
    summaryPriority (head s) `shouldBe` 100
    summaryCount (head s) `shouldBe` 1
    summaryCountWorking (head s) `shouldBe` 0

  it "groups by class name" $ \c -> do
    insertJob (defaultArgs { aClass = "foo" }) c
    insertJob (defaultArgs { aClass = "foo" }) c
    insertJob (defaultArgs { aClass = "bar" }) c
    length <$> queueSummary c "" `shouldReturn` 2

  it "groups by priority" $ \c -> do
    insertJob (defaultArgs { aClass = "foo", aPriority = 1 }) c
    insertJob (defaultArgs { aClass = "foo", aPriority = 2 }) c
    length <$> queueSummary c "" `shouldReturn` 2

  it "only returns results for the given queue" $ \c -> do
    insertJob (defaultArgs { aQueue = "a" }) c
    insertJob (defaultArgs { aQueue = "b" }) c
    length <$> queueSummary c "" `shouldReturn` 0
    length <$> queueSummary c "a" `shouldReturn` 1
    length <$> queueSummary c "b" `shouldReturn` 1

testJob :: Spec
testJob = around withDb $ do
  it "returns a job with all the right properties" $ \c -> do
    id_ <- insertJob defaultArgs c
    Just j <- job c id_
    jobId j `shouldBe` id_
    jobPriority j `shouldBe` 100
    jobClass j `shouldBe` "someClass"
    jobArgs j `shouldBe` Array mempty
    jobFailed j `shouldBe` False
    jobFailedAt j `shouldBe` Nothing
    jobScheduledForFuture j `shouldBe` False

  it "returns Nothing if there's no job with the given id" $ \c ->
    job c 0 `shouldReturn` Nothing

testRetryJob :: Spec
testRetryJob = around withDb $ do
  it "does nothing if there's no job with the given id" $ \c ->
    retryJob c 0 `shouldReturn` Nothing

  it "retries the job with the given id" $ \c -> do
    i <- insertJob (defaultArgs { aRetryable = False }) c
    fmap jobRetryable <$> job c i `shouldReturn` Just False
    retryJob c i
    fmap jobRetryable <$> job c i `shouldReturn` Just True

testDestroyJob :: Spec
testDestroyJob = around withDb $ do
  it "does nothing if there's no job with the given id" $ \c ->
    destroyJob c 0 `shouldReturn` False

  it "destroys the job with the given id" $ \c -> do
    i <- insertJob defaultArgs c
    isJust <$> job c i `shouldReturn` True
    destroyJob c i
    isJust <$> job c i `shouldReturn` False

testRetryFailures :: Spec
testRetryFailures = around withDb $ do
  it "does nothing if there are no jobs of the given class" $ \c -> do
    i <- insertJob (defaultArgs { aClass = "foo", aRetryable = False }) c
    before <- job c i
    retryFailures c "bar"
    after <- job c i
    before `shouldBe` after

  it "does nothing if no jobs of the given class have failed" $ \c -> do
    a <- insertJob (defaultArgs { aClass = "foo", aRetryable = True }) c
    b <- insertJob (defaultArgs { aClass = "foo", aRetryable = False, aErrorCount = 0 }) c
    retryFailures c "foo"
    fmap jobRetryable <$> job c a `shouldReturn` Just True
    fmap jobRetryable <$> job c b `shouldReturn` Just False

  it "does nothing if the failed jobs are currently being worked" $ \c ->
    -- TODO
    True `shouldBe` True

  it "retries failed jobs of the given class" $ \c -> do
    a <- insertJob (defaultArgs { aClass = "foo", aRetryable = False, aErrorCount = 1 }) c
    fmap jobRetryable <$> job c a `shouldReturn` Just False
    retryFailures c "foo"
    fmap jobRetryable <$> job c a `shouldReturn` Just True

testDestroyFailures :: Spec
testDestroyFailures = around withDb $ do
  it "does nothing if there are no jobs of the given class" $ \c -> do
    i <- insertJob (defaultArgs { aClass = "foo", aRetryable = False }) c
    destroyFailures c "bar"
    isJust <$> job c i `shouldReturn` True

  it "does nothing if no jobs of the given class have failed" $ \c -> do
    a <- insertJob (defaultArgs { aClass = "foo", aRetryable = True }) c
    b <- insertJob (defaultArgs { aClass = "foo", aRetryable = False, aErrorCount = 0 }) c
    destroyFailures c "foo"
    isJust <$> job c a `shouldReturn` True
    isJust <$> job c b `shouldReturn` True

  it "does nothing if the failed jobs are currently being worked" $ \c ->
    -- TODO
    True `shouldBe` True

  it "destroys failed jobs of the given class" $ \c -> do
    a <- insertJob (defaultArgs { aClass = "foo", aRetryable = False, aErrorCount = 1 }) c
    isJust <$> job c a `shouldReturn` True
    destroyFailures c "foo"
    job c a `shouldReturn` Nothing

testFailureSummary :: Spec
testFailureSummary = around withDb $ do
  it "returns nothing when there are no failed jobs" $ \c -> do
    insertJob defaultArgs c
    failureSummary c `shouldReturn` []

  it "returns failed jobs, grouped by job class" $ \c -> do
    insertJob defaultArgs c
    insertJob (defaultArgs { aClass = "foo", aRetryable = False }) c
    insertJob (defaultArgs { aClass = "foo", aErrorCount = 1 }) c
    insertJob (defaultArgs { aClass = "bar", aRetryable = False }) c
    s <- failureSummary c
    length s `shouldBe` 2

  it "returns the count of inactive jobs" $ \c -> do
    insertJob (defaultArgs { aRetryable = False }) c
    insertJob (defaultArgs { aRetryable = False }) c
    map failureInactive <$> failureSummary c `shouldReturn` [2]

  it "returns the count of pending retry jobs" $ \c -> do
    insertJob (defaultArgs { aErrorCount = 1 }) c
    insertJob (defaultArgs { aErrorCount = 1 }) c
    map failurePendingRetry <$> failureSummary c `shouldReturn` [2]

  it "orders by the number of errors" $ \c -> do
    insertJob (defaultArgs { aClass = "foo", aErrorCount = 1 }) c
    insertJob (defaultArgs { aClass = "bar", aErrorCount = 3 }) c
    insertJob (defaultArgs { aClass = "baz", aErrorCount = 2 }) c
    map failureClass <$> failureSummary c `shouldReturn` ["bar", "baz", "foo"]

testWorkers :: Spec
testWorkers = around withDb $ do
  it "returns an empty list if there are no jobs being worked" $ \c -> do
    insertJob defaultArgs c
    workers c `shouldReturn` []

  it "returns a list of jobs currently being worked" $ \c -> do
    a <- insertJob defaultArgs c
    workJob c a $ length <$> workers c `shouldReturn` 1

data JobArgs = JobArgs { aPriority   :: Int
                       , aClass      :: Text
                       , aQueue      :: Text
                       , aErrorCount :: Int
                       , aRetryable  :: Bool
                       }
defaultArgs :: JobArgs
defaultArgs = JobArgs { aPriority = 100
                      , aClass = "someClass"
                      , aQueue = ""
                      , aErrorCount = 0
                      , aRetryable = True
                      }

insertJob :: JobArgs -> Connection -> IO Int
insertJob args c = do
  let as = (aPriority args, aClass args, aQueue args, aErrorCount args, aRetryable args)
  [Only i] <- query c [sql|
    INSERT INTO que_jobs (priority, job_class, queue, error_count, retryable)
    VALUES (?, ?, ?, ?, ?)
    RETURNING job_id
  |] as
  return i

-- Simulate a job being worked by taking an advisory lock against it, and then
-- run the given action
workJob :: Connection -> Int -> IO a -> IO a
workJob c i action = bracket_
  (query c [sql| SELECT pg_try_advisory_lock(?) |] [i] :: IO [ Only Bool ])
  (query c [sql| SELECT pg_advisory_unlock(?) |] [i] :: IO [ Only Bool ])
  action

-- create the que_jobs table
migrate :: Connection -> IO ()
migrate c = do
  _ <- execute_ c [sql|
    CREATE SEQUENCE que_jobs_job_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
  |]

  _ <- execute_ c [sql|
    CREATE TABLE que_jobs (
      job_id bigint not null default nextval('que_jobs_job_id_seq'::regclass),
      priority smallint not null default 100,
      run_at timestamp with time zone not null default now(),
      job_class text not null,
      args json not null default '[]'::json,
      error_count integer not null default 0,
      last_error text,
      queue text not null default ''::text,
      retryable boolean default true,
      failed_at timestamp with time zone
    )
  |]
  return ()
