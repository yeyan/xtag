{-# LANGUAGE TemplateHaskell #-}

module XTag.Config
    ( readConfig
    , repositories
    , cacheRoot
    , port
    , redisConnectInfo
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

import qualified Data.ByteString.UTF8       as U
import           Database.Redis.Simple      (ConnectInfo (..), PortID (..),
                                             defaultConnectInfo)
--import           Network.Socket             (PortNumber (..))

type Port = Int

data Config = Config
    { _repositories     :: [ FilePath ]
    , _cacheRoot        :: FilePath
    , _port             :: Port
    , _redisConnectInfo :: ConnectInfo
    }

defaultConfig =
    Config ["../data"] "../cache" 3000 defaultConnectInfo

makeLenses ''Config

makeLensesFor
    [ ("connectHost", "connectHostL")
    , ("connectPort", "connectPortL")
    , ("connectAuth", "connectAuthL")
    , ("connectMaxConnections", "connectMaxConnectionsL")
    , ("connectMaxIdleTime", "connectMaxIdelTimeL")
    ] ''ConnectInfo

readConfig file = do
    exists <- doesFileExist file
    if exists
        then do
            result <- flip runStateT defaultConfig $ runEitherT $ do
                result <- EitherT $ liftIO $ parseConfig file
                let repos = result ^. at "gallery_repositories"
                    val x = read . head <$> result ^. at x
                    portN = val "server_port"
                    cache = val "thumbnail_cache_root"
                    cHost = val "redis_connect_host"
                    cPort = PortNumber <$> fromIntegral <$> val "redis_connect_port"
                    cAuth = Just <$> (U.fromString . head)
                        <$> result ^. at "redis_authentication"
                    maxConn = val "redis_max_connection"
                    maxIdle = fromIntegral <$> val "redis_max_idle_seconds"
                update repositories repos
                update cacheRoot cache
                update port portN
                update (redisConnectInfo . connectHostL) cHost
                update (redisConnectInfo . connectPortL) cPort
                update (redisConnectInfo . connectAuthL) cAuth
                update (redisConnectInfo . connectMaxConnectionsL) maxConn
                update (redisConnectInfo . connectMaxIdelTimeL) maxIdle
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

