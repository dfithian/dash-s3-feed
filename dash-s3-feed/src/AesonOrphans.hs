{-# OPTIONS_GHC -fno-warn-orphans #-}
module AesonOrphans where

import Control.Lens.TH (makeWrapped)
import Network.AWS.S3 (BucketName, ObjectKey)

makeWrapped ''BucketName
makeWrapped ''ObjectKey
