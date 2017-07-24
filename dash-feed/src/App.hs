module App where

import ClassyPrelude hiding (Handler)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Logger (askLoggerIO, logInfo, runLoggingT)
import Data.Aeson (eitherDecodeStrict')
import Data.Pool (createPool, destroyAllResources)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.AWS (Credentials(Discover), newEnv)
import Network.Wai.Handler.Warp (run)
import Servant ((:<|>)((:<|>)))
import Servant.Server ((:~>)(NT), Handler, enter, serve)

import Api (api)
import File (getFile, postFile)
import Foundation (App(App), MonadStack, _appConnectionPool, _appSettings)
import Logging (LogFunction, withLogger)
import Settings (Settings(Settings), _settingsDatabase, _settingsPoolSize, _settingsPort)

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
      let port = _settingsPort _appSettings
      $logInfo $ "Starting server on " <> tshow port
      liftIO . run port . serve api . enter (stackToHandler app logFunc) $
        getFile :<|> postFile
