{-# LANGUAGE TemplateHaskell #-}

module XTag.Config
    ( readConfig
    , configRepos
    , configCache
    , configPort
    , Port
    , Config
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Either

import           Data.Map.Lens
import           Data.Maybe
import           XTag.Config.Parser

import           System.Directory
import           Text.Parsec.Error

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

readConfig file = do
    exists <- doesFileExist file
    if exists
        then do
            result <- flip runStateT defaultConfig $ runEitherT $ do
                result <- EitherT $ liftIO $ parseConfig file
                let repos = result ^. at "repos"
                    val x = read . head <$> result ^. at x
                    portN = val "port"
                    cache = val "cache"
                update configRepos repos
                update configCache cache
                update configPort  portN
            return $ _1 . _Left %~ show $ result
        else
            return (Left fileNotFound, defaultConfig)
    where
        update lens v = do
            case v of
                Nothing -> return ()
                Just vl -> lens .= vl
        fileNotFound =
                "Could not found configuration file \"" ++ file ++ "\""

