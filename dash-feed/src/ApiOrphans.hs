{-# OPTIONS_GHC -fno-warn-orphans #-}
module ApiOrphans where

import Composite.Record ((:->)(Val))
import Network.AWS.S3 (BucketName(BucketName), ObjectKey(ObjectKey))
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

import Types (FBucket, FKey, FManifestKey)

deriving instance ToHttpApiData BucketName
deriving instance ToHttpApiData ObjectKey
deriving instance ToHttpApiData FBucket
deriving instance ToHttpApiData FKey
deriving instance ToHttpApiData FManifestKey
deriving instance FromHttpApiData BucketName
deriving instance FromHttpApiData ObjectKey
deriving instance FromHttpApiData FBucket
deriving instance FromHttpApiData FKey
deriving instance FromHttpApiData FManifestKey
