{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Config
    ( App(..)
    , Config(..)
    , Environment(..)
    , PartySession
    , defaultConfig
    , envPool
    , envSetCorsOrigin
    , envAugmentSessionCookie
    , envSessionCookieSecure
    , envManager
    , lookupSetting
    , setLogger
    ) where

import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import           Data.ByteString                      (ByteString)
import           Data.Maybe                           (fromJust, isNothing)
import           Data.Text                            (Text, pack)
import qualified Data.Vault.Lazy                      as Vault
import           Database.Persist.Postgresql          (ConnectionPool)
import           Network.HTTP.Client                  (Manager, ManagerSettings (managerConnCount, managerResponseTimeout),
                                                       newManager,
                                                       responseTimeoutMicro)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Network.Wai.Session                  as WS
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)
import           System.Exit                          (die)
import           System.IO.Unsafe                     (unsafePerformIO)
import qualified Web.Cookie                           as C

import           Middleware.Cors                      (getCorsPolicy)
import           Network.Spotify                      as Spotify
import           Utils                                (strToBS)

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
    { getPool                 :: ConnectionPool
    , getPort                 :: Port
    , getEnv                  :: Environment
    , getCorsOrigins          :: String
    , getVaultKey             :: Vault.Key PartySession
    , getManager              :: Manager
    , getSpotifyCallback      :: String
    , getSpotifyAuthorization :: Spotify.TokenAuthorization
    } deriving (Show)

instance Show (Vault.Key PartySession) where
    show _ = "Vault.Key PartySession"
instance Show Manager where
    show _ = "HTTP.Client Manager"

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
    , getCorsOrigins = "http://chancesnow.me"
    , getVaultKey = setVaultKey
    , getManager = undefined
    , getSpotifyCallback = setSpotifyCallback
    , getSpotifyAuthorization = setSpotifyAuthorization
    }

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

{-# NOINLINE setSpotifyCallback #-}
setSpotifyCallback :: String
setSpotifyCallback = unsafePerformIO $ do
    maybeSpotifyCallback <- lookupEnv "SPOTIFY_CALLBACK"
    let spotifyCallback = map Just (fromJust maybeSpotifyCallback)
    return $ removeTrailingSlash spotifyCallback where
        removeTrailingSlash str = case last str of
            Just '/' -> map fromJust (init str)
            _        -> map fromJust str

{-# NOINLINE setVaultKey #-}
setVaultKey :: Vault.Key PartySession
setVaultKey = unsafePerformIO Vault.newKey

{-# NOINLINE setSpotifyAuthorization #-}
setSpotifyAuthorization :: Spotify.TokenAuthorization
setSpotifyAuthorization = authorization where
    authorization = unsafePerformIO $ do
        spotifyClientId <- lookupEnv "SPOTIFY_APP_KEY"
        spotifySecretKey <- lookupEnv "SPOTIFY_APP_SECRET"
        -- If either key doesn't exist, die
        if authKeysExist spotifyClientId spotifySecretKey
            then return Spotify.TokenAuthorization
                { clientId = pack $ fromJust spotifyClientId
                , clientSecretKey = pack $ fromJust spotifySecretKey
                }
            else die (
                "One or both required Spotify Web API keys are non-existent\n"
                ++ "\tEnsure proper configuration exists in the environment.\n\n"
                ++ "\t(See .env.example)")

authKeysExist :: Maybe String -> Maybe String -> Bool
authKeysExist spotifyClientId spotifySecretKey =
    not (isNothing spotifyClientId || isNothing spotifySecretKey)

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

envSetCorsOrigin :: Environment -> String -> Middleware
envSetCorsOrigin _ corsOrigins = Cors.cors $ getCorsPolicy (Just corsOrigins)

envAugmentSessionCookie :: Config -> C.SetCookie -> C.SetCookie
envAugmentSessionCookie cfg setCookie =
    setCookie
        { C.setCookieDomain = Just $
            strToBS $ envSessionCookieDomain (getEnv cfg)
        }

envSessionCookieDomain :: Environment -> String
envSessionCookieDomain Production = ".chancesnow.me"
envSessionCookieDomain _          = ".app.local"

envSessionCookieSecure :: Environment -> Bool
envSessionCookieSecure Production = True
envSessionCookieSecure _          = False

envManager :: Environment -> IO Manager
envManager env = newManager $ envManagerSettings env

envManagerSettings :: Environment -> ManagerSettings
envManagerSettings Test        = tlsManagerSettings
    { managerConnCount = 10
    , managerResponseTimeout = responseTimeoutMicro (2 * 1250000) -- 2.5 seconds
    }
envManagerSettings Development = tlsManagerSettings
    { managerConnCount = 10
    , managerResponseTimeout = responseTimeoutMicro (3 * 1000000) -- 3 seconds
    }
envManagerSettings Production  = tlsManagerSettings
    { managerConnCount = 10
    , managerResponseTimeout = responseTimeoutMicro (5 * 1000000) -- 5 seconds
    }
