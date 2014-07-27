{-# LANGUAGE TemplateHaskell #-}

module XTag.Config
    ( readConfig
    , configRepos
    , configCache
    , configPort
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Either

import           Data.Map.Lens
import           Data.Maybe
import           XTag.Config.Parser

type Port = Int

data Config = Config
    { _configRepos :: [ FilePath ]
    , _configCache :: FilePath
    , _ConfigPort  :: Port
    }
    deriving (Show)

defaultConfig =
    Config ["../data"] "../cache" 3000

makeLenses ''Config

readConfig file =
    flip runStateT defaultConfig $ runEitherT $ do
        result <- EitherT $ liftIO $ parseConfig file
        let repos = result ^. at "repositories"
            val x = read . head <$> result ^. at x
            portN = val "port"
            cache = val "cache"
        update configRepos repos
        update configCache cache
        update configPort  portN
    where
        update lens v = do
            case v of
                Nothing -> return ()
                Just vl -> lens .= vl
