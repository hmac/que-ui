{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Lib where

import           Data.Aeson
import           Data.ByteString.Read        (integral)
import           Data.Int                    (Int64)
import           Data.Int                    (Int8)
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import           Data.Time
import           Data.Word                   (Word8)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics

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

mkPersist sqlSettings [persistLowerCase|
Lock sql=pg_locks json
  locktype String     sqltype=text
  database Oid Maybe      sqltype=oid
  relation Oid Maybe      sqltype=oid
  page Int8 Maybe           sqltype=integer
  tuple Int Maybe           sqltype=smallint
  virtualxid String Maybe   sqltype=text
  transactionid Xid Maybe sqltype=xid
  classid Oid Maybe       sqltype=oid
  objid Oid Maybe sqltype=oid
  objsubid Int Maybe sqltype=smallint
  Id String sql=virtualtransaction sqltype=text
  pid Int8 Maybe sqltype=integer
  mode String sqltype=text
  granted Bool
  fastpath Bool
  deriving Show
|]

newtype Oid = Oid Word8 deriving (Show, Generic)
instance PersistField Oid where
  toPersistValue oid = PersistDbSpecific $ (fromString . show) oid
  fromPersistValue (PersistDbSpecific s) = case integral s of
                                             Just (n, _) -> Right $ Oid n
                                             _      -> Left "Invalid OID"
  fromPersistValue _ = Left "Invalid OID (not PersistDbSpecific)"
instance PersistFieldSql Oid where
  sqlType _ = SqlOther "oid"
instance FromJSON Oid
instance ToJSON Oid where
  toEncoding = genericToEncoding defaultOptions

newtype Xid = Xid Word8 deriving (Show, Generic)
instance PersistField Xid where
  toPersistValue xid = PersistDbSpecific $ (fromString . show) xid
  fromPersistValue (PersistDbSpecific s) = case integral s of
                                             Just (n, _) -> Right $ Xid n
                                             _      -> Left "Invalid xid"
  fromPersistValue _ = Left "Invalid xid (not PersistDbSpecific)"
instance PersistFieldSql Xid where
  sqlType _ = SqlOther "xid"
instance FromJSON Xid
instance ToJSON Xid where
  toEncoding = genericToEncoding defaultOptions
