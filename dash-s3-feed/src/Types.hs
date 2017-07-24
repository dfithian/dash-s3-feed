module Types where

import ClassyPrelude
import Composite.Aeson
  ( JsonFormatRec, field, jsonFormatWithIso, wrappedJsonFormat
  , iso8601DateTimeJsonFormat, textJsonFormat )
import Composite.Aeson.TH (makeRecJsonWrapperExplicit)
import Composite.Record (Rec((:&), RNil), (:->))
import Composite.TH (withLensesAndProxies)
import Control.Lens (iso)
import Control.Lens.TH (makeWrapped)
import Network.AWS.S3 (BucketName, ObjectKey)

import AesonOrphans ()

withLensesAndProxies [d|
  type FCreated = "created"         :-> UTCTime
  type FBucket = "bucket"           :-> BucketName
  type FKey = "key"                 :-> ObjectKey
  type FManifestKey = "manifestKey" :-> ObjectKey

  type FName = "name"               :-> ByteString
  type FPassword = "password"       :-> ByteString
  |]

type File = '[FCreated, FBucket, FKey, FManifestKey]
type User = '[FName, FPassword]

fileFormat :: JsonFormatRec e File
fileFormat = field iso8601DateTimeJsonFormat
  :& field (wrappedJsonFormat textJsonFormat)
  :& field (wrappedJsonFormat textJsonFormat)
  :& field (wrappedJsonFormat textJsonFormat)
  :& RNil

userFormat :: JsonFormatRec e User
userFormat = field (jsonFormatWithIso (iso decodeUtf8 encodeUtf8) textJsonFormat)
  :& field (jsonFormatWithIso (iso decodeUtf8 encodeUtf8) textJsonFormat)
  :& RNil

makeRecJsonWrapperExplicit "FileJson" ''File [|fileFormat|]
makeRecJsonWrapperExplicit "UserJson" ''User [|userFormat|]
makeWrapped ''FileJson
makeWrapped ''UserJson
