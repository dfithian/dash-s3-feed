module Settings where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data Settings = Settings
  { _settingsDatabase       :: Text
  , _settingsPoolSize       :: Int
  , _settingsPort           :: Int
  , _settingsCallbackHost   :: Text
  , _settingsCallbackScheme :: Text
  }
makeLenses ''Settings

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \ obj -> Settings
    <$> obj .: "database"
    <*> obj .: "pool-size"
    <*> obj .: "port"
    <*> obj .: "callback-host"
    <*> obj .: "callback-scheme"
