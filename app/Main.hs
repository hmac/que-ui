{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import           Database.PostgreSQL.Simple
import           Network.HTTP.Types.Status
import           Sql                        (healthCheck, queueSummary, workers)
import           Web.Scotty

main :: IO ()
main = do
  conn <- connectPostgreSQL ""
  scotty 3001 $ do
    get "/health_check" (healthCheckRoute conn)
    get "/queue-summary/:queue" (queueSummaryRoute conn)
    get "/queue-summary" (redirect "/queue-summary/")
    get "/workers" (workersRoute conn)

healthCheckRoute :: Connection -> ActionM ()
healthCheckRoute conn = do
  s <- liftIO (healthCheck conn)
  json s

queueSummaryRoute :: Connection -> ActionM ()
queueSummaryRoute conn = do
  queue <- safeParam "queue"
  case queue of
    Nothing -> status status400
    Just q -> do
      summary <- liftIO $ queueSummary conn q
      json summary

workersRoute :: Connection -> ActionM ()
workersRoute conn = do
  ws <- liftIO $ workers conn
  json ws

jobsRoute :: Connection -> ActionM ()
jobsRoute conn = do
  -- priority <- safeParam "priority"
  -- jobClass <- safeParam "job_class"
  -- queue <- safeParam "queue"
  -- errorCount <- safeParam "error_count"
  return ()

-- TODO: this is dangerous and lame
constructWhere :: [(Text, Text)] -> Text
constructWhere ws = T.append " WHERE " (go ws)
  where go :: [(Text, Text)] -> Text
        go []                       = ""
        go ((column, value) : rest) = T.concat [column, " = ", value, go rest]

-- Like `param`, but when a parameter isn't present it
-- returns Nothing instead of raising an exception.
safeParam :: Parsable a => L.Text -> ActionM (Maybe a)
safeParam p = fmap Just (param p) `rescue` const (return Nothing)
