{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Middleware.Flash
    ( FlashedData(..)
    , flash
    , flashData
    , flashMiddleware
    , fromFlashData
    , getFlashedData
    , getFlashedError
    ) where

import           Control.Monad.Reader (liftIO)
import           Data.Aeson           (FromJSON, ToJSON, decode, encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Map             (Map)
import           Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Vault.Lazy      as Vault
import           GHC.Generics         (Generic)
import           Network.Wai          (Middleware)
import qualified Network.Wai          as W

import           Config               (App (..), Config (..), PartySession)
import           Middleware.Session   (SessionState (SessionAvailable),
                                       getSession, getSession')
import           Utils                (strToBS)

data FlashedData = FlashedData
    { isFresh :: Bool
    , getData :: Map String String
    } deriving (Generic, Show, Read)

flashData :: Map String String -> FlashedData
flashData dataToFlash = FlashedData { isFresh = True, getData = dataToFlash }

fromFlashData :: FlashedData -> Map String String
fromFlashData = getData

instance ToJSON FlashedData
instance FromJSON FlashedData

flash :: PartySession -> Map String String -> IO ()
flash session mapToFlash = flash' session $ flashData mapToFlash

flash' :: PartySession -> FlashedData -> IO ()
flash' (_, sessionInsert) dataToFlash =
    replaceFlash sessionInsert (toStrict $ encode dataToFlash)

getFlashedError :: Vault.Vault -> App (Maybe String)
getFlashedError vault = do
    maybeFlashedData <- getFlashedData vault
    return $ case maybeFlashedData of
        Just flashedData -> case M.lookup "error" flashedData of
            Just err -> Just err
            Nothing  -> Nothing
        Nothing -> Nothing

getFlashedData :: Vault.Vault -> App (Maybe (Map String String))
getFlashedData vault = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable (sessionLookup, _) -> do
            maybeFlashData <- liftIO $ lookupFlashData sessionLookup
            case maybeFlashData of
                Just flashedData -> return $ Just (getData flashedData)
                Nothing          -> return Nothing
        _                                   -> return Nothing

flashMiddleware :: Config -> Middleware
flashMiddleware cfg = removeFlashMiddleware $ getSession' (getVaultKey cfg)

removeFlashMiddleware :: (Vault.Vault -> IO SessionState) -> Middleware
removeFlashMiddleware getSessionState app req sendResponse =
    app req $ \res -> do
        sessionState <- getSessionState (W.vault req)
        let respond = sendResponse res
        case sessionState of
            SessionAvailable (sessionLookup, sessionInsert) -> do
                maybeFlashData <- lookupFlashData sessionLookup
                case maybeFlashData of
                    Just flashedData -> do
                        if isFresh flashedData
                            then replaceFlash sessionInsert $
                                toStrict $ encode FlashedData
                                    { isFresh = False
                                    , getData = getData flashedData
                                    }
                            else removeFlash sessionInsert
                        respond
                    _ -> respond
            _ -> respond

lookupFlashData :: (Text -> IO (Maybe ByteString)) -> IO (Maybe FlashedData)
lookupFlashData sessionLookup = do
    maybeFlashDataBS <- sessionLookup "FLASH"
    case maybeFlashDataBS of
        Just flashDataBS -> do
            let maybeFlashData = decode $ fromStrict flashDataBS
                    :: Maybe FlashedData
            return maybeFlashData
        Nothing -> return Nothing

removeFlash :: (Text -> ByteString -> IO ()) -> IO ()
removeFlash sessionInsert = replaceFlash sessionInsert (strToBS "")

replaceFlash :: (Text -> ByteString -> IO ()) -> ByteString -> IO ()
replaceFlash sessionInsert = sessionInsert "FLASH"
