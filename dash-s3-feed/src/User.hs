module User where

import ClassyPrelude
import Composite.Record ((:->)(Val))
import Control.Lens (view)
import Opaleye (runQuery)
import Servant (BasicAuthData(BasicAuthData))
import Servant.Server
  ( BasicAuthCheck(BasicAuthCheck)
  , BasicAuthResult(Authorized, BadPassword, NoSuchUser)
  , Context((:.), EmptyContext) )

import Foundation (App, runDb)
import Model (selectUser)
import Types (UserJson(UserJson), fPassword)

basicAuthContext :: App -> Context (BasicAuthCheck UserJson ': '[])
basicAuthContext app = authCheck app :. EmptyContext

authCheck :: App -> BasicAuthCheck UserJson
authCheck app = BasicAuthCheck $ \ (BasicAuthData name password) -> do
  users <- runReaderT (runDb . flip runQuery $ selectUser (Val name)) app
  case headMay users of
    Nothing -> pure NoSuchUser
    Just badUser | view fPassword badUser /= password -> pure BadPassword
    Just user -> pure $ Authorized $ UserJson user
