module Types where

import ClassyPrelude
import Composite.Aeson
  ( JsonFormatRec, field, wrappedJsonFormat
  , iso8601DateTimeJsonFormat, textJsonFormat )
import Composite.Aeson.TH (makeRecJsonWrapperExplicit)
import Composite.Record (Rec((:&), RNil), (:->))
import Composite.TH (withLensesAndProxies)
import Control.Lens.TH (makeWrapped)
import Network.AWS.S3 (BucketName, ObjectKey)

import AesonOrphans ()

withLensesAndProxies [d|
  type FCreated = "created"           :-> UTCTime
  type FBucket = "bucket"             :-> BucketName
  type FKey = "key"                   :-> ObjectKey
  type FManifestKey = "manifestKey"   :-> ObjectKey
  |]

type File = '[FCreated, FBucket, FKey, FManifestKey]

fileFormat :: JsonFormatRec e File
fileFormat = field iso8601DateTimeJsonFormat
  :& field (wrappedJsonFormat textJsonFormat)
  :& field (wrappedJsonFormat textJsonFormat)
  :& field (wrappedJsonFormat textJsonFormat)
  :& RNil

makeRecJsonWrapperExplicit "FileJson" ''File [|fileFormat|]
makeWrapped ''FileJson
