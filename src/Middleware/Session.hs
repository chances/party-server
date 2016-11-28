{-# LANGUAGE OverloadedStrings #-}

module Middleware.Session
    ( SessionState(..)
    , sessionMiddleware
    , startSession
    , getSession
    , invalidateSession
    ) where

import           Control.Monad.Reader                 (ask, liftIO)
import           Data.Int                             (Int64)
import qualified Data.Vault.Lazy                      as Vault
import           Network.Wai                          (Middleware)
import           Web.ServerSession.Backend.Persistent (SqlStorage (..))
import           Web.ServerSession.Frontend.Wai       (ForceInvalidate (AllSessionIdsOfLoggedUser),
                                                       forceInvalidate,
                                                       setAuthKey,
                                                       setCookieName,
                                                       withServerSession)

import           Config                               (App (..), Config (..),
                                                       PartySession)
import           Utils                                (strToBS)

sessionMiddleware :: Config -> IO Middleware
sessionMiddleware cfg = do
    let sessionKey = getVaultKey cfg
        sessionStorage = SqlStorage (getPool cfg)
        sessionOptions = setAuthKey "ID" . setCookieName "cpSESSION"
    withServerSession sessionKey sessionOptions sessionStorage :: IO Middleware

getSession :: Vault.Vault -> App (Maybe PartySession)
getSession vault = do
    cfg <- ask :: App Config
    return $ Vault.lookup (getVaultKey cfg) vault

data SessionState =
    SessionStarted PartySession
  | SessionInvalidated
  | SessionDoesNotExist

invalidateSession :: Vault.Vault -> App SessionState
invalidateSession vault = do
    maybeSession <- getSession vault
    case maybeSession of
        Nothing      -> return SessionDoesNotExist
        Just session -> liftIO $ do
            forceInvalidate session AllSessionIdsOfLoggedUser
            return SessionInvalidated

startSession :: Vault.Vault -> Int64 -> App SessionState
startSession vault authId = do
    maybeSession <- getSession vault
    case maybeSession of
        Nothing -> return SessionDoesNotExist
        Just session -> liftIO $ do
            let (_, sessionInsert) = session
                key = strToBS $ show authId
            sessionInsert "ID" key
            return $ SessionStarted session
