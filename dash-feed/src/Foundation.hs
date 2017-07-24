module Foundation where

import ClassyPrelude
import Control.Lens (view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Servant.Server (ServantErr)
import Network.AWS (AWS, Env, HasEnv, Region(NorthVirginia), environment, runAWS, within)

import Settings (Settings)

type MonadStack r = ExceptT ServantErr (LoggingT (ReaderT r IO))

data App = App
  { _appConnectionPool :: Pool Connection
  , _appEnv            :: Env
  , _appSettings       :: Settings
  }
makeLenses ''App

instance HasEnv App where environment = appEnv

runDb :: (MonadBaseControl IO m, MonadReader App m) => (Connection -> IO a) -> m a
runDb action = do
  pool <- view appConnectionPool
  liftBase $ withResource pool action

runAws :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadReader r m, HasEnv r)
       => AWS a -> m a
runAws action = do
  env <- view environment
  runResourceT . runAWS env . within NorthVirginia $ action
