module Api where

import ClassyPrelude
import Control.Monad.Except (MonadError, throwError)
import Data.Proxy (Proxy(Proxy))
import Servant ((:<|>), (:>), Capture, Get, JSON, OctetStream, Post, ServantErr)
import Servant.Server (err400, errBody)
import Servant.Multipart (MultipartForm, MultipartData)

import ApiOrphans ()
import Types (FBucket, FKey, FManifestKey)

api :: Proxy API
api = Proxy

type API = "file" :> (
  Capture "bucket" FBucket :> Capture "key" FKey :> Get '[OctetStream] ByteString
  :<|> Capture "bucket" FBucket :> Capture "key" FManifestKey :> MultipartForm MultipartData :> Post '[JSON] ()
  )

badRequest :: MonadError ServantErr m => Text -> m a
badRequest reason = throwError $ err400 { errBody = encodeUtf8 $ fromStrict reason }
