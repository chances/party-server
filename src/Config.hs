{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
    ( App(..)
    , Config(..)
    , Environment(..)
    , defaultConfig
    , envPool
    , envSetCorsOrigin
    , lookupSetting
    , setLogger
    ) where

import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import           Database.Persist.Postgresql          (ConnectionPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

import           Middleware.Cors                      (getCorsPolicy)

newtype App a = App
    { runApp :: ReaderT Config (ExceptT ServantErr IO) a } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadError ServantErr
        , MonadIO
        )

data Config = Config
    { getPool       :: ConnectionPool
    , getPort       :: Port
    , getEnv        :: Environment
    , getCorsOrigin :: String
    }

data Environment =
      Development
    | Test
    | Production
    deriving (Eq, Show, Read)

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getPort = 8080
    , getEnv = Development
    , getCorsOrigin = "http://chancesnow.me"
    }

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

envSetCorsOrigin :: Environment -> String -> Middleware
envSetCorsOrigin Test corsOrigin       = Cors.cors $ getCorsPolicy (Just corsOrigin)
envSetCorsOrigin Development _         = Cors.cors $ getCorsPolicy Nothing
envSetCorsOrigin Production corsOrigin = Cors.cors $ getCorsPolicy (Just corsOrigin)
