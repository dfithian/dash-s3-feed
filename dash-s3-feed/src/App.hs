module App where

import ClassyPrelude hiding (Handler)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Logger (askLoggerIO, runLoggingT)
import Data.Aeson (eitherDecodeStrict')
import Data.Pool (createPool, destroyAllResources)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.AWS (Credentials(Discover), newEnv)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant ((:<|>)((:<|>)))
import Servant.Server ((:~>)(NT), Handler, enter, serveWithContext)

import Api (api)
import File (getFile, postFile)
import Foundation (App(App), MonadStack, _appConnectionPool, _appSettings)
import Logging (LogFunction, withLogger)
import Settings
  ( Settings(Settings), _settingsCertificateKeyPath, _settingsCertificatePath
  , _settingsDatabase, _settingsPoolSize, _settingsPort )
import User (basicAuthContext)

stackToHandler' :: r -> LogFunction -> MonadStack r a -> Handler a
stackToHandler' site logFunc action =
  either throwError pure =<< liftIO (runReaderT (runLoggingT (runExceptT action) logFunc) site)

stackToHandler :: r -> LogFunction -> MonadStack r :~> Handler
stackToHandler site logFunc = NT $ stackToHandler' site logFunc

makeApp :: IO App
makeApp = do
  settings@Settings {..} <- either fail pure . eitherDecodeStrict'
    =<< readFile "config/settings.json"
  App
    <$> createPool (connectPostgreSQL $ encodeUtf8 _settingsDatabase) close 1 20 _settingsPoolSize
    <*> newEnv Discover
    <*> pure settings

startApp :: IO ()
startApp = do
  app@App {..} <- makeApp
  withLogger $
    flip finally (liftIO $ destroyAllResources _appConnectionPool) $ do
      logFunc <- askLoggerIO
      let tls = tlsSettings (_settingsCertificatePath _appSettings) (_settingsCertificateKeyPath _appSettings)
          warp = setPort (_settingsPort _appSettings) defaultSettings
      liftIO . runTLS tls warp . serveWithContext api (basicAuthContext app)
        . enter (stackToHandler app logFunc)
        $ \ _ -> getFile :<|> postFile
