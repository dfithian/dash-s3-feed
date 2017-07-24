module File where

import ClassyPrelude
import Composite.Record (Record, Rec(RNil), (:->)(Val), pattern (:*:))
import Control.Lens (to, view)
import Control.Monad.Logger (logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkLbs, sourceLbs)
import Data.Default (def)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.AWS (send)
import Network.AWS.Data.Body (RqBody(Chunked), ChunkedBody(ChunkedBody), _streamBody)
import qualified Network.AWS.S3 as S3
import Network.HTTP.Types (urlEncode)
import Opaleye (constant, runInsertMany)
import Servant.Multipart (MultipartData, fdFilePath, lookupFile, lookupInput)
import Text.XML (renderLBS)
import Text.XML.Writer (content, document, element)

import Api (badRequest)
import Foundation (App, MonadStack, appSettings, runAws, runDb)
import Model (FileCols, fileTable)
import Settings (settingsCallbackHost, settingsCallbackScheme, settingsPort)
import Types (File, FBucket, FKey, FManifestKey)

getObject :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadReader App m)
          => S3.BucketName -> S3.ObjectKey -> m ByteString
getObject bucket key = do
  contents <- runAws $ do
    response <- send $ S3.getObject bucket key
    liftIO . runResourceT $ view (S3.gorsBody . to _streamBody) response $$+- sinkLbs
  pure $ toStrict contents

putObject :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadReader App m)
          => S3.BucketName -> S3.ObjectKey -> ByteString -> m ()
putObject bucket key contents =
  runAws . void . send $ S3.putObject bucket key
    . Chunked . ChunkedBody 100 (toInteger $ length contents) . sourceLbs . fromStrict
    $ contents

-- |Return a file by the provided key
getFile :: FBucket -> FKey -> MonadStack App ByteString
getFile (Val bucket) (Val key) = do
  $logDebug $ "Retrieving file at " <> tshow bucket <> ", " <> tshow key
  getObject bucket key

createCallbackUrl :: MonadReader App m => S3.BucketName -> S3.ObjectKey -> m Text
createCallbackUrl (S3.BucketName bucket) (S3.ObjectKey key) = do
  scheme <- view (appSettings . settingsCallbackScheme)
  host <- view (appSettings . settingsCallbackHost)
  port <- view (appSettings . settingsPort)
  pure $ scheme <> "://" <> host <> ":" <> tshow port
    <> "/file/" <> bucket
    <> "/" <> (decodeUtf8 . urlEncode False . encodeUtf8) key

-- |Push a file and manifest at the provided manifest key
postFile :: FBucket -> FManifestKey -> MultipartData -> MonadStack App ()
postFile (Val bucket) (Val manifestKey) multipartData = do
  now <- liftIO getCurrentTime
  uuid <- toText <$> liftIO nextRandom
  fileVersion <- maybe (badRequest "Version required") pure $ lookupInput "version" multipartData
  fileData <- maybe (badRequest "File required") pure $ lookupFile "file" multipartData
  file <- liftIO . readFile . fdFilePath $ fileData
  let fileKey = S3.ObjectKey $ uuid <> "/" <> fileVersion <> ".tar.gz"
  url <- createCallbackUrl bucket fileKey
  let manifest = toStrict . renderLBS def $ document "entry" $ do
        element "version" $ content fileVersion
        element "url" $ content url
      row = (constant :: Record File -> Record FileCols) $ now :*: bucket :*: fileKey :*: manifestKey :*: RNil
  runDb $ \ conn -> void $ runInsertMany conn fileTable [row]
  putObject bucket manifestKey manifest
  putObject bucket fileKey file
