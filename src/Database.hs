{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database
  (
    Connection -- re-exported from PostgreSQL.Simple
  , forkConnectionMonitor
  )
  where

import           Control.Concurrent         (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception          (try)
import           Database.PostgreSQL.Simple

-- A sleep duration, which is a wrapper around an integer of microseconds
newtype SleepDuration = SleepDuration { toInt :: Int }

defaultInitialBackoff :: SleepDuration
defaultInitialBackoff = SleepDuration { toInt = 100000 }

defaultBackoff :: SleepDuration
defaultBackoff = SleepDuration { toInt = 1000000 }

forkConnectionMonitor :: MVar Connection -> IO ThreadId
forkConnectionMonitor conn = forkIO $ do
  establishConnection defaultInitialBackoff conn
  dbKeepalive defaultBackoff conn

-- Establish a database connection and store it in the given (empty) MVar
establishConnection :: SleepDuration -> MVar Connection -> IO ()
establishConnection sleep connVar = do
  conn <- monitorConnection sleep id
  putMVar connVar conn
  return ()

-- Poll the database connection, checking that it is still up
-- If it isn't, attempt to reconnect, backing off exponentially
dbKeepalive :: SleepDuration -> MVar Connection -> IO ()
dbKeepalive sleep connVar = do
  threadSleep sleep
  c <- readMVar connVar
  (alive :: Either IOError [Only Int]) <- try (query_ c "SELECT 1")
  case alive of
    Right _ -> dbKeepalive sleep connVar
    Left _  -> reEstablishConnection sleep connVar

-- Reconnect to the database and store the connection in the given MVar,
-- replacing its previous contents.
reEstablishConnection :: SleepDuration -> MVar Connection -> IO ()
reEstablishConnection sleep connVar = do
  conn <- monitorConnection sleep (\d -> SleepDuration $ toInt d * 2)
  _ <- swapMVar connVar conn
  return ()

-- Attempt to connect to the database, backing off using the given backoff function.
monitorConnection :: SleepDuration -> (SleepDuration -> SleepDuration) -> IO Connection
monitorConnection sleep backoff = do
  threadSleep sleep
  (c :: Either IOError Connection) <- try (connectPostgreSQL "")
  case c of
    Left _   -> threadSleep sleep >> monitorConnection (backoff sleep) backoff
    Right c' -> return c'

-- Equivalent to threadDelay, but takes a SleepDuration as an argument
threadSleep :: SleepDuration -> IO ()
threadSleep d = threadDelay (toInt d)
