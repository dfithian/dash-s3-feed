module User where

import ClassyPrelude
import Composite.Record ((:->)(Val), Record)
import Control.Monad.Logger (logWarn)
import Crypto.KDF.BCrypt (validatePassword)
import Control.Lens (view)
import Data.Vinyl.Lens (rcast)
import Opaleye (runQuery)
import Servant (BasicAuthData(BasicAuthData))
import Servant.Server
  ( BasicAuthCheck(BasicAuthCheck)
  , BasicAuthResult(Authorized, BadPassword)
  , Context((:.), EmptyContext) )

import Foundation (App, runDb)
import Logging (withLogger)
import Model (selectUser)
import Types (User, UserJson(UserJson), fPassword)

basicAuthContext :: App -> Context (BasicAuthCheck UserJson ': '[])
basicAuthContext app = authCheck app :. EmptyContext

authCheck :: App -> BasicAuthCheck UserJson
authCheck app = BasicAuthCheck $ \ (BasicAuthData name password) -> withLogger $ do
  users <- runReaderT (runDb . flip runQuery $ selectUser (Val name)) app
  case headMay users of
    Nothing -> do
      $logWarn $ "User " <> decodeUtf8 name <> " not found"
      pure BadPassword
    Just (user :: Record User) -> do
      case validatePassword password $ view fPassword user of
        True -> pure . Authorized . UserJson . rcast $ user
        False -> do
          $logWarn $ "User " <> decodeUtf8 name <> " failed with incorrect password"
          pure BadPassword
