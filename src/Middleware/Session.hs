{-# LANGUAGE OverloadedStrings #-}

module Middleware.Session
    ( SessionState(..)
    , getSession
    , getSession'
    , getSessionOrDie
    , getUserFromSession
    , getUsernameFromSession
    , invalidateSession
    , popOffSession
    , sessionMiddleware
    , renameCookieDomainMiddleware
    , startSession
    ) where

import qualified Blaze.ByteString.Builder             as Builder
import           Control.Monad.Reader                 (ask, liftIO)
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, pack)
import qualified Data.Vault.Lazy                      as Vault
import           Database.Persist.Postgresql          (Entity (..), SqlPersistT,
                                                       getBy)
import           Network.HTTP.Types.Header            (Header)
import           Network.Wai                          (Middleware, Response,
                                                       mapResponseHeaders,
                                                       responseHeaders)
import           Servant                              (throwError)
import qualified Web.Cookie                           as C
import           Web.ServerSession.Backend.Persistent (SqlStorage (..))
import           Web.ServerSession.Frontend.Wai       (ForceInvalidate (AllSessionIdsOfLoggedUser),
                                                       forceInvalidate,
                                                       setAuthKey,
                                                       setCookieName,
                                                       setHttpOnlyCookies,
                                                       setSecureCookies,
                                                       withServerSession)

import           Api.Envelope                         (fromServantError)
import           Config                               (App (..), Config (..),
                                                       PartySession,
                                                       envAugmentSessionCookie,
                                                       envSessionCookieSecure)
import           Database.Models
import           Database.Party                       (runDb)
import           Utils                                (bsToStr, noSessionError,
                                                       strToBS)

sessionMiddleware :: Config -> IO Middleware
sessionMiddleware cfg = do
    let sessionKey = getVaultKey cfg
        sessionStorage = SqlStorage (getPool cfg)
        sessionOptions = setAuthKey "ID" .
            setCookieName "cpSESSION" .
            setHttpOnlyCookies False .
            (setSecureCookies $ envSessionCookieSecure (getEnv cfg))
    withServerSession sessionKey sessionOptions sessionStorage :: IO Middleware

renameCookieDomainMiddleware :: Config -> Middleware
renameCookieDomainMiddleware cfg app request sendResponse =
    app request $ sendResponse . setCookieDomain where
        setCookieDomain :: Response -> Response
        setCookieDomain response =
            let headers = responseHeaders response
                newHeaders =
                    replaceHeaders cfg headers setCookieSessionHeader
                in mapResponseHeaders (\_ -> newHeaders) response

replaceHeaders :: Config -> [Header] ->
    (Config -> Header -> Maybe Header) -> [Header]
replaceHeaders cfg headers headerFilter =
    map (\header ->
        case headerFilter cfg header of
            Just newHeader -> newHeader
            Nothing        -> header
    ) headers

setCookieSessionHeader :: Config -> Header -> Maybe Header
setCookieSessionHeader cfg (headerName, headerValue) =
    if headerName == "Set-Cookie"
        then let
            setCookie = C.parseSetCookie headerValue
            in case C.setCookieName setCookie of
                "cpSESSION" -> Just $ let
                    newSetCookie =
                        envAugmentSessionCookie cfg setCookie
                    newHeaderValue = Builder.toByteString
                        (C.renderSetCookie newSetCookie)
                    in (headerName, newHeaderValue)
                _ -> Nothing
        else Nothing

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

getSession' :: Vault.Key PartySession -> Vault.Vault -> IO SessionState
getSession' vaultKey vault = do
    let maybeSession = Vault.lookup vaultKey vault
    case maybeSession of
        Nothing      -> return SessionDoesNotExist
        Just session -> return $ SessionAvailable session

getSessionOrDie :: Vault.Vault -> App PartySession
getSessionOrDie vault = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable session -> return session
        _ -> throwError $ fromServantError noSessionError

getUsernameFromSession :: Vault.Vault -> App (Maybe String)
getUsernameFromSession vault = do
    (sessionLookup, _) <- getSessionOrDie vault
    maybeAuthUsername <- liftIO $ sessionLookup sessionUserIdKey
    return $ case maybeAuthUsername of
        Just user -> Just $ bsToStr user
        Nothing   -> Nothing

getUserFromSession :: Vault.Vault -> App (Either SessionState User)
getUserFromSession vault = do
    maybeAuthUsername <- getUsernameFromSession vault
    let authUsername = fromMaybe "" maybeAuthUsername
        getUserByUsername = getBy $ UniqueUsername authUsername
            :: SqlPersistT IO (Maybe (Entity User))
    case compare authUsername "" of
        EQ -> return $ Left SessionInvalidated
        _ -> do
            maybeUser <- runDb getUserByUsername
            case maybeUser of
                Just (Entity _ user) -> return $ Right user
                Nothing              -> return $ Left SessionInvalidated

invalidateSession :: Vault.Vault -> App SessionState
invalidateSession vault = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable session -> liftIO $ do
            let (_, sessionInsert) = session
            forceInvalidate session AllSessionIdsOfLoggedUser
            sessionInsert sessionUserIdKey ""
            return SessionInvalidated
        _ -> return sessionState

startSession :: Vault.Vault -> String -> App SessionState
startSession vault authUsername = do
    sessionState <- getSession vault
    case sessionState of
        SessionAvailable session -> liftIO $ do
            let (_, sessionInsert) = session
                key = strToBS authUsername
            sessionInsert "ID" key
            sessionInsert sessionUserIdKey key
            return $ SessionStarted session
        _ -> return sessionState

sessionUserIdKey :: Text
sessionUserIdKey = "USER"

popOffSession :: PartySession -> String -> String -> IO String
popOffSession session key defaultValue = do
    let (sessionLookup, sessionInsert) = session
    maybeReturnTo <- sessionLookup (pack key)
    case maybeReturnTo of
        Just returnToBs -> do
            let returnTo = bsToStr returnToBs
            -- If the value is empty return defaultValue, otherwise pop the key
            case returnTo of
                "" -> return defaultValue
                _ -> do
                    sessionInsert (pack key) (strToBS "")
                    return returnTo
        _ -> return defaultValue
