{-# LANGUAGE OverloadedStrings #-}

module Middleware.Session
    ( SessionState(..)
    , sessionMiddleware
    , startSession
    , getSession
    , getSessionOrDie
    , invalidateSession
    ) where

import           Control.Monad.Reader                 (ask, liftIO)
import           Data.Int                             (Int64)
import qualified Data.Vault.Lazy                      as Vault
import           Network.Wai                          (Middleware)
import           Servant                              (throwError)
import           Web.ServerSession.Backend.Persistent (SqlStorage (..))
import           Web.ServerSession.Frontend.Wai       (ForceInvalidate (AllSessionIdsOfLoggedUser),
                                                       forceInvalidate,
                                                       setAuthKey,
                                                       setCookieName,
                                                       withServerSession)

import           Api.Envelope                         (fromServantError)
import           Config                               (App (..), Config (..),
                                                       PartySession)
import           Utils                                (noSessionError, strToBS)

sessionMiddleware :: Config -> IO Middleware
sessionMiddleware cfg = do
    let sessionKey = getVaultKey cfg
        sessionStorage = SqlStorage (getPool cfg)
        sessionOptions = setAuthKey "ID" . setCookieName "cpSESSION"
    withServerSession sessionKey sessionOptions sessionStorage :: IO Middleware

data SessionState =
    SessionStarted PartySession
  | SessionAvailable PartySession
  | SessionInvalidated
  | SessionDoesNotExist

getSession :: Vault.Vault -> App SessionState
getSession vault = do
    cfg <- ask :: App Config
    let maybeSession = Vault.lookup (getVaultKey cfg) vault
    case maybeSession of
        Nothing      -> return SessionDoesNotExist
        Just session -> return $ SessionAvailable session

getSessionOrDie :: Vault.Vault -> App PartySession
getSessionOrDie vault = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable session -> return session
        _ -> throwError $ fromServantError noSessionError

invalidateSession :: Vault.Vault -> App SessionState
invalidateSession vault = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable session -> liftIO $ do
            forceInvalidate session AllSessionIdsOfLoggedUser
            return SessionInvalidated
        _ -> return sessionState

startSession :: Vault.Vault -> Int64 -> App SessionState
startSession vault authId = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable session -> liftIO $ do
            let (_, sessionInsert) = session
                key = strToBS $ show authId
            sessionInsert "ID" key
            return $ SessionStarted session
        _ -> return sessionState
