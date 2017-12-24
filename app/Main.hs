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
import           Text.Printf                         (printf)
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
jobsRoute :: MVar Connection -> ActionM ()
jobsRoute conn = do
  priority <- fmap Priority <$> safeParam "priority"
  jobClass <- fmap Class <$> safeParam "job_class"
  queue <- fmap Queue <$> safeParam "queue"
  failed <- safeParam "failed"
  let filters = catMaybes [priority, jobClass, queue]
  case failed of
    Just True -> failedJobsRoute conn filters
    _         -> notFailedJobsRoute conn filters

failedJobsRoute :: MVar Connection -> [Filter] -> ActionM ()
failedJobsRoute conn _filters = do
  c <- liftIO $ readMVar conn
  js <- liftIO $ failedJobs c
  json js

notFailedJobsRoute :: MVar Connection -> [Filter] -> ActionM ()
notFailedJobsRoute conn _filters = do
  c <- liftIO $ readMVar conn
  js <- liftIO $ jobs c
  json js

-- TODO: this is dangerous and lame
constructWhere :: [Text] -> Text
constructWhere [] = ""
constructWhere (c : cs) = T.concat [" WHERE ", c, " = ?", go cs]
  where go :: [Text] -> Text
        go []       = ""
        go (d : ds) = T.concat [" AND ", d, " = ?", go ds]

-- Like `param`, but when a parameter isn't present it
-- returns Nothing instead of raising an exception.
safeParam :: Parsable a => L.Text -> ActionM (Maybe a)
safeParam p = fmap Just (param p) `rescue` const (return Nothing)
