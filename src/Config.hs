{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
    ( App(..)
    , Config(..)
    , Environment(..)
    , PartySession
    , defaultConfig
    , envPool
    , envSetCorsOrigin
    , lookupSetting
    , setLogger
    ) where

import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import           Data.ByteString                      (ByteString)
import           Data.Text                            (Text)
import qualified Data.Vault.Lazy                      as Vault
import           Database.Persist.Postgresql          (ConnectionPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Network.Wai.Session                  as WS
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)
import           System.IO.Unsafe                     (unsafePerformIO)

import           Middleware.Cors                      (getCorsPolicy)

type PartySession = WS.Session IO Text ByteString
-- type PartySession = Session SessionMap
-- type PartySession = Session IO Text ByteString
-- Session is a (sessionLookup, sessionInsert) pair
-- (key -> m (Maybe value), key -> value -> m ())
-- (lookup                       , insert)
-- (Text -> IO (Maybe ByteString), Text -> ByteString -> IO ())

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
    , getVaultKey   :: Vault.Key PartySession
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
    , getVaultKey = setVaultKey
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

setVaultKey :: Vault.Key PartySession
setVaultKey = unsafePerformIO $ Vault.newKey
