{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Control.Monad.State

import qualified Data.Text.Lazy        as T
import           Database.Redis.Simple
import           Web.Scotty.Trans

data AppConfig = AppConfig
    { _repositories :: [String]
    , _cache        :: String
    }
    deriving (Show)

data AppData = AppData
    { _config     :: AppConfig
    , _connection :: Connection
    }

makeLenses ''AppConfig
makeLenses ''AppData

type XTagScottyM = ScottyT T.Text (StateT AppData IO)
type XTagActionM = ScottyT T.Text (StateT AppData IO)

type Port = Int

defaultAppConfig =
    AppConfig ["data"] "cache"

test = AppData (AppConfig ["100"] "cachedir") undefined
