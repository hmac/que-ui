{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.MVar
import           Control.Exception                    (throw)
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.Map.Strict                      as Map
import qualified Data.Text.Lazy                       as L
import           Database                             (dbKeepalive,
                                                       defaultBackoff,
                                                       defaultInitialBackoff,
                                                       establishConnection)
import           Database.PostgreSQL.Simple
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Sql                                  (JobFilter (..),
                                                       destroyFailures,
                                                       destroyJob,
                                                       failureSummary,
                                                       healthCheck, job, jobs,
                                                       queueSummary,
                                                       retryFailures, retryJob,
                                                       workers)
import           System.IO                            (BufferMode (LineBuffering),
                                                       hSetBuffering, stdout)
import           System.Remote.Monitoring             (forkServer)
import           Web.Scotty

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "starting Que UI..."
  conn <- newEmptyMVar
  _ <- forkIO $ do
    establishConnection defaultInitialBackoff conn
    dbKeepalive defaultBackoff conn
  _ <- forkServer "localhost" 8081
  app conn

app :: MVar Connection -> IO ()
app conn = scotty 8080 $ do
    middleware logStdout
    get "/health_check" $ withCors (healthCheckRoute conn)
    get "/queue-summary/:queue" $ withCors (queueSummaryRoute conn)
    get "/queue-summary" $ withCors (redirect "/queue-summary/")
    get "/failures" $ withCors (failureSummaryRoute conn)
    get "/failures/:job_class" $ withCors (failuresRoute conn)
    post "/failures/:job_class/retry" $ withCors (retryFailuresRoute conn)
    post "/failures/:job_class/destroy" $ withCors (destroyFailuresRoute conn)
    get "/workers" $ withCors (workersRoute conn)
    get "/jobs/:id" $ withCors (jobRoute conn)
    post "/jobs/:id/retry" $ withCors (retryJobRoute conn)
    post "/jobs/:id/destroy" $ withCors (destroyJobRoute conn)
    get "/jobs" $ withCors (jobsRoute conn)

    -- Static files
    get "/" $ file "./client/index.html"
    get (literal "/app.js") $ file "./client/app.js"
    get (literal "/css/app.css") $ file "./client/css/app.css"
    get "/css/:file" $ param "file" >>= \f -> file ("./client/css/" ++ f)

-- The type for all our API routes
-- Every route has access to the database connection
type Route = MVar Connection -> ActionM ()

withCors :: ActionM () -> ActionM ()
withCors a = setHeader "Access-Control-Allow-Origin" "*" >> a

healthCheckRoute :: Route
healthCheckRoute conn = do
  c <- liftIO $ readMVarNow conn
  up <- liftIO $ healthCheck c
  json $ Map.insert "db" up (Map.singleton "api" True :: Map.Map L.Text Bool)

queueSummaryRoute :: Route
queueSummaryRoute conn = do
  c <- liftIO $ readMVarNow conn
  queue <- safeParam "queue"
  case queue of
    Nothing -> status status404
    Just "_default" -> do
      summary <- liftIO $ queueSummary c ""
      json summary
    Just q -> do
      summary <- liftIO $ queueSummary c q
      json summary

failureSummaryRoute :: Route
failureSummaryRoute conn = do
  c <- liftIO $ readMVarNow conn
  summary <- liftIO $ failureSummary c
  json summary

failuresRoute :: Route
failuresRoute conn = do
  jobClass <- safeParam "job_class"
  case jobClass of
    Nothing -> status status404
    Just jc -> do
      let f = JobFilter { filterPriority = Nothing
                        , filterClass = Just jc
                        , filterQueue = Nothing
                        , filterFailed = Just True
                        }
      c <- liftIO $ readMVarNow conn
      failures <- liftIO $ jobs f c
      json failures

retryFailuresRoute :: Route
retryFailuresRoute conn = do
  jobClass <- safeParam "job_class"
  case jobClass of
    Nothing -> status status404
    Just jc -> do
      c <- liftIO $ readMVarNow conn
      j <- liftIO $ retryFailures c jc
      json j

destroyFailuresRoute :: Route
destroyFailuresRoute conn = do
  jobClass <- safeParam "job_class"
  case jobClass of
    Nothing -> status status404
    Just jc -> do
      c <- liftIO $ readMVarNow conn
      j <- liftIO $ destroyFailures c jc
      json j

workersRoute :: Route
workersRoute conn = do
  c <- liftIO $ readMVarNow conn
  ws <- liftIO $ workers c
  json ws

jobRoute :: Route
jobRoute conn = do
  jobId <- safeParam "id"
  case jobId of
    Nothing -> status status404
    Just i -> do
      c <- liftIO $ readMVarNow conn
      j <- liftIO $ job c i
      json j

retryJobRoute :: Route
retryJobRoute conn = do
  jobId <- safeParam "id"
  case jobId of
    Nothing -> status status404
    Just i -> do
      c <- liftIO $ readMVarNow conn
      j <- liftIO $ retryJob c i
      json j

destroyJobRoute :: Route
destroyJobRoute conn = do
  jobId <- safeParam "id"
  case jobId of
    Nothing -> status status404
    Just i -> do
      c <- liftIO $ readMVarNow conn
      _ <- liftIO $ destroyJob c i
      json ()

jobsRoute :: Route
jobsRoute conn = do
  priority <- safeParam "priority"
  jobClass <- safeParam "job_class"
  queue <- safeParam "queue"
  failed <- safeParam "failed"
  let f = JobFilter { filterPriority = priority
                     , filterClass = jobClass
                     , filterQueue = queue
                     , filterFailed = failed
                     }
  c <- liftIO $ readMVarNow conn
  js <- liftIO $ jobs f c
  json js

-- Like `param`, but when a parameter isn't present it
-- returns Nothing instead of raising an exception.
safeParam :: Parsable a => L.Text -> ActionM (Maybe a)
safeParam p = fmap Just (param p) `rescue` const (return Nothing)

-- Attempt to read the given MVar, and throw an exception if the read would block
-- We use this when reading the database connection so that we'll throw an
-- exception if we get an API request before the connection has been
-- established.
readMVarNow :: MVar a -> IO a
readMVarNow m = do
  x <- tryReadMVar m
  case x of
    Just x' -> return x'
    Nothing -> throw $ userError "attempted to read empty mvar"

