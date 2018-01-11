{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception          (throw, try)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Text.Lazy             as L
import           Database.PostgreSQL.Simple
import           Network.HTTP.Types.Status
import           Sql                        (JobFilter (..), failureSummary,
                                             healthCheck, job, jobs,
                                             queueSummary, retryJob, workers)
import           System.IO                  (BufferMode (LineBuffering),
                                             hSetBuffering, stdout)
import           Web.Scotty

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "starting Que UI..."
  conn <- newEmptyMVar
  _ <- forkIO $ do
    establishConnection 100000 conn
    dbKeepalive 1000000 conn -- 1 second sleep by default
  app conn

-- Attempt to connect to the database, backing off using the given backoff function.
monitorConnection :: Int -> (Int -> Int) -> IO Connection
monitorConnection sleep backoff = do
  threadDelay sleep
  (c :: Either IOError Connection) <- try (connectPostgreSQL "")
  case c of
    Left _   -> threadDelay sleep >> monitorConnection (backoff sleep) backoff
    Right c' -> return c'

-- Establish a database connection and store it in the given (empty) MVar
establishConnection :: Int -> MVar Connection -> IO ()
establishConnection sleep connVar = do
  conn <- monitorConnection sleep id
  putMVar connVar conn
  return ()

-- Reconnect to the database and store the connection in the given MVar,
-- replacing its previous contents.
reEstablishConnection :: Int -> MVar Connection -> IO ()
reEstablishConnection sleep connVar = do
  conn <- monitorConnection sleep (* 2)
  _ <- swapMVar connVar conn
  return ()

-- Poll the database connection, checking that it is still up
-- If it isn't, attempt to reconnect, backing off exponentially
dbKeepalive :: Int -> MVar Connection -> IO ()
dbKeepalive sleep connVar = do
  threadDelay sleep
  c <- readMVar connVar
  (alive :: Either IOError [Only Int]) <- try (query_ c "SELECT 1")
  case alive of
    Right _ -> dbKeepalive sleep connVar
    Left _  -> reEstablishConnection sleep connVar

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

withCors :: ActionM () -> ActionM ()
withCors a = do
  setHeader "Access-Control-Allow-Origin" "*"
  a

app :: MVar Connection -> IO ()
app conn = scotty 8080 $ do
    get "/health_check" $ withCors(healthCheckRoute conn)
    get "/queue-summary/:queue" $ withCors (queueSummaryRoute conn)
    get "/queue-summary" $ withCors (redirect "/queue-summary/")
    get "/failures" $ withCors (failureSummaryRoute conn)
    get "/workers" $ withCors (workersRoute conn)
    get "/jobs/:id" $ withCors (jobRoute conn)
    post "/jobs/:id/retry" $ withCors (retryJobRoute conn)
    get "/jobs" $ withCors (jobsRoute conn)


    -- Static files
    -- TODO: fix this crap
    -- Local (non-docker) development:
    get "/" $ file "./client/index.html"
    get (literal "/app.js") $ file "./client/app.js"
    get (literal "/css/app.css") $ file "./client/css/app.css"
    get "/css/:file" $ param "file" >>= \f -> file ("./client/css/" ++ f)

    -- Production
    -- get (literal "/elm.js") $ file "/opt/app/client/elm.js"
    -- get (literal "/css/app.css") $ file "opt/app/client/css/app.css"
    -- get "/css/:file" $ param "file" >>= \f -> file ("opt/app/client/css/" ++ f)

    -- Old stuff
    -- get (literal "/vendor/js/system.js") $ file "/opt/app/client/vendor/js/system.js"
    -- get (literal "/config.js") $ file "/opt/app/client/config.js"
    -- get (literal "/build.js") $ file "/opt/app/client/build.js"
    -- get (literal "/css/app.css") $ file "./client/css/app.css"
    -- get "/css/:file" $ param "file" >>= \f -> file ("./client/css/" ++ f)
    -- get "/js/:file" $ param "file" >>= \f -> file ("/opt/app/client/js/" ++ f)
    -- get "/vendor/js/:file" $ param "file" >>= \f -> file ("/opt/app/client/vendor/js/" ++ f)

healthCheckRoute :: MVar Connection -> ActionM ()
healthCheckRoute conn = do
  c <- liftIO $ readMVarNow conn
  s <- liftIO $ healthCheck c
  json s

queueSummaryRoute :: MVar Connection -> ActionM ()
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

failureSummaryRoute :: MVar Connection -> ActionM ()
failureSummaryRoute conn = do
  c <- liftIO $ readMVarNow conn
  summary <- liftIO $ failureSummary c
  json summary

workersRoute :: MVar Connection -> ActionM ()
workersRoute conn = do
  c <- liftIO $ readMVarNow conn
  ws <- liftIO $ workers c
  json ws

jobRoute :: MVar Connection -> ActionM ()
jobRoute conn = do
  jobId <- safeParam "id"
  case jobId of
    Nothing -> status status404
    Just i -> do
      c <- liftIO $ readMVarNow conn
      j <- liftIO $ job c i
      json j

retryJobRoute :: MVar Connection -> ActionM ()
retryJobRoute conn = do
  jobId <- safeParam "id"
  case jobId of
    Nothing -> status status404
    Just i -> do
      c <- liftIO $ readMVarNow conn
      j <- liftIO $ retryJob c i
      json j


jobsRoute :: MVar Connection -> ActionM ()
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
  js <- liftIO $ jobs c f
  json js


-- Like `param`, but when a parameter isn't present it
-- returns Nothing instead of raising an exception.
safeParam :: Parsable a => L.Text -> ActionM (Maybe a)
safeParam p = fmap Just (param p) `rescue` const (return Nothing)
