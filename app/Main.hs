{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                  (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception                   (SomeException (..), catch,
                                                      throw, try)
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.ByteString                     as B
import           Data.Maybe                          (isJust)
import           Data.Maybe                          (catMaybes)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as L
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Internal (libPQError)
import           Network.HTTP.Types.Status
import           Network.Wai                         (Request)
import           Network.Wai.Handler.Warp            (defaultSettings,
                                                      setOnException, setPort)
import           Sql                                 (failedJobs, healthCheck,
                                                      jobs, queueSummary,
                                                      workers)
import           Web.Scotty

main :: IO ()
main = do
  conn <- connectPostgreSQL "" >>= newMVar
  forkIO (dbKeepalive 1000000 conn) -- 1 second sleep by default
  app conn `catch` \(e :: IOError) -> do
    putStrLn "caught error"
    print e
    return ()

-- Poll the database connection, checking that it is still up
-- If it isn't, attempt to reconnect it, backing off exponentially
dbKeepalive :: Int -> MVar Connection -> IO ()
dbKeepalive sleep connVar = do
  threadDelay sleep
  c <- readMVar connVar
  (alive :: Either IOError [Only Int]) <- try (query_ c "SELECT 1")
  case alive of
    Right _ -> dbKeepalive sleep connVar
    Left _ -> do
      (res :: Either IOError Connection) <- try (connectPostgreSQL "")
      case res of
        Left _ -> dbKeepalive (sleep * 2) connVar
        Right c' -> do
          swapMVar connVar c'
          dbKeepalive sleep connVar

app :: MVar Connection -> IO ()
app conn = scottyOpts opts $ do
    get "/health_check" (healthCheckRoute conn)
    get "/queue-summary/:queue" (queueSummaryRoute conn)
    get "/queue-summary" (redirect "/queue-summary/")
    get "/workers" (workersRoute conn)
    get "/jobs" (jobsRoute conn)
    get "/raise" $ (liftIO . throw) (libPQError "db exploded")
      where opts = Options 0 settings
            settings = setPort 3001 . setOnException handleError $ defaultSettings

handleError :: Maybe Request -> SomeException -> IO ()
handleError _req err = do
  putStrLn "an error occurred!"
  putStrLn $ "Error: " ++ show err

healthCheckRoute :: MVar Connection -> ActionM ()
healthCheckRoute conn = do
  c <- liftIO $ readMVar conn
  s <- liftIO $ healthCheck c
  json s

queueSummaryRoute :: MVar Connection -> ActionM ()
queueSummaryRoute conn = do
  c <- liftIO $ readMVar conn
  queue <- safeParam "queue"
  case queue of
    Nothing -> status status400
    Just q -> do
      summary <- liftIO $ queueSummary c q
      json summary

workersRoute :: MVar Connection -> ActionM ()
workersRoute conn = do
  c <- liftIO $ readMVar conn
  ws <- liftIO $ workers c
  json ws

data Filter = Priority Text | Class Text | Queue Text deriving Eq
data JobsFilter = JobsFilter
  { filterPriority :: Maybe Int
  , filterClass    :: Maybe Text
  , filterQueue    :: Maybe Text
  , filterFailed   :: Maybe Bool
  }
jobsRoute :: MVar Connection -> ActionM ()
jobsRoute conn = do
  priority <- safeParam "priority"
  jobClass <- safeParam "job_class"
  queue <- safeParam "queue"
  failed <- safeParam "failed"
  let f = JobsFilter { filterPriority = priority, filterClass = jobClass, filterQueue = queue, filterFailed = failed }
  c <- liftIO $ readMVar conn
  js <- liftIO $ jobs c (constructFilter f)
  json js

constructFilter :: JobsFilter -> (Bool, Maybe Int, Bool,
                                  Bool, Maybe Text, Bool,
                                  Bool, Maybe Text, Bool,
                                  Bool, Maybe Bool, Bool)
constructFilter filter = (byPriority, p, byPriority, byClass, c, byClass, byQueue, q, byQueue, byFailed, f, byFailed)
  where p = filterPriority filter
        c = filterClass filter
        q = filterQueue filter
        f = filterFailed filter
        byPriority = isJust p
        byClass = isJust c
        byQueue = isJust q
        byFailed = isJust f

-- Like `param`, but when a parameter isn't present it
-- returns Nothing instead of raising an exception.
safeParam :: Parsable a => L.Text -> ActionM (Maybe a)
safeParam p = fmap Just (param p) `rescue` const (return Nothing)
