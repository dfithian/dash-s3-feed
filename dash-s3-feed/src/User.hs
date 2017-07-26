module User where

import ClassyPrelude
import Composite.Record ((:->)(Val), Record)
import Crypto.KDF.BCrypt (validatePassword)
import Control.Lens (view)
import Data.Vinyl.Lens (rcast)
import Opaleye (runQuery)
import Servant (BasicAuthData(BasicAuthData))
import Servant.Server
  ( BasicAuthCheck(BasicAuthCheck)
  , BasicAuthResult(Authorized, BadPassword, NoSuchUser)
  , Context((:.), EmptyContext) )

import Foundation (App, runDb)
import Model (selectUser)
import Types (User, UserJson(UserJson), fPassword)

basicAuthContext :: App -> Context (BasicAuthCheck UserJson ': '[])
basicAuthContext app = authCheck app :. EmptyContext

authCheck :: App -> BasicAuthCheck UserJson
authCheck app = BasicAuthCheck $ \ (BasicAuthData name password) -> do
  users <- runReaderT (runDb . flip runQuery $ selectUser (Val name)) app
  case headMay users of
    Nothing -> pure NoSuchUser
    Just (user :: Record User) -> do
      case validatePassword password $ view fPassword user of
        True -> pure . Authorized . UserJson . rcast $ user
        False -> pure BadPassword
