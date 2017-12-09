{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Lib where

import           Data.Int                    (Int64)
import qualified Data.Text                   as T
import           Data.Time
import           Database.Persist.Postgresql
import           Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
QueJob sql=que_jobs json
  priority Int sqltype=smallint
  runAt UTCTime
  Id Int64 sqltype=bgint sql=job_id
  jobClass String sqltype=text
  args T.Text sqltype=json
  errorCount Int sqltype=integer
  lastError T.Text Maybe sqltype=text
  queue String sqltype=text
  retryable Bool Maybe
  failedAt UTCTime Maybe
  deriving Show
|]
