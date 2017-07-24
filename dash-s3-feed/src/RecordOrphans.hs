{-# OPTIONS_GHC -fno-warn-orphans#-}
module RecordOrphans where

import Control.Lens (lmap, view)
import Data.Profunctor.Product.Default (Default, def)
import Database.PostgreSQL.Simple.FromField (FromField)
import Network.AWS.S3 (BucketName(BucketName), ObjectKey(ObjectKey), _ObjectKey)
import Opaleye (Column, Constant, PGText, QueryRunnerColumnDefault)

deriving instance FromField BucketName
deriving instance FromField ObjectKey
deriving instance QueryRunnerColumnDefault PGText BucketName
deriving instance QueryRunnerColumnDefault PGText ObjectKey
instance Default Constant BucketName (Column PGText) where
  def = lmap (\ (BucketName b) -> b) def
instance Default Constant ObjectKey (Column PGText) where
  def = lmap (view _ObjectKey) def
