{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Api.Auth
    ( AuthAPI
    , authServer
    ) where

import           Control.Monad          (unless)
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.Reader   (ask, liftIO)
import           Data.ByteString        (ByteString)
import           Data.Map               as M
import           Data.Text              (Text, pack)
import           Servant
import           Servant.Common.BaseUrl (showBaseUrl)

import           Api.Envelope           (fromServantError)
import           Config                 (App (..), Config (getManager),
                                         PartySession)
import           Middleware.Flash       (flash)
import           Middleware.Session     (SessionState (SessionInvalidated),
                                         getSessionOrDie, invalidateSession,
                                         popOffSession)
import qualified Spotify.Api.Types      as Spotify
import           Spotify.Client         (authorizeLink, spotifyAccountsBaseUrl,
                                         tokenRequest)
import           Utils                  (badRequest, baseUrl, bsToStr,
                                         noSessionError, serverError, strToBS)

-- HTTP 301 MOVED PERMANENTLY (i.e. redirect)
type GetMoved contentTypes a = Verb 'GET 301 contentTypes a
-- HTTP 302 FOUND (i.e. redirect "temporarily")
type GetFound contentTypes a = Verb 'GET 302 contentTypes a

data HTML
instance Accept HTML where
    contentType _ = "text/html;charset=utf-8"

type RedirectHeaders = (Headers '[Header "Location" String] NoContent)
type Redirect =
    GetFound '[PlainText] RedirectHeaders

type LoginAPI = "login"
    :> Vault :> QueryParam "return_to" String
    :> GetFound '[PlainText] (Headers '[Header "Location" String] NoContent)
type LoginCallbackAPI = "callback"
    :> Vault
    :> QueryParam "code" Spotify.AuthorizationCode
    :> QueryParam "error" String
    :> QueryParam "state" Spotify.State
    :> Redirect
type LogoutAPI   = "logout"
    :> Vault :> Redirect

getLoginLink :: HasLink LoginAPI => MkLink LoginAPI
getLoginLink = safeLink (Proxy :: Proxy AuthAPI) (Proxy :: Proxy LoginAPI)

instance HasLink sub => HasLink (Vault :> sub) where
    type MkLink (Vault :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)

getCallbackLink :: HasLink LoginCallbackAPI => MkLink LoginCallbackAPI
getCallbackLink = safeLink (Proxy :: Proxy AuthAPI) (Proxy :: Proxy LoginCallbackAPI)

callbackLink :: String
callbackLink = showBaseUrl baseUrl ++ "/auth/" ++ show (getCallbackLink Nothing Nothing Nothing)

getLogoutLink :: HasLink LogoutAPI => MkLink LogoutAPI
getLogoutLink = safeLink (Proxy :: Proxy AuthAPI) (Proxy :: Proxy LogoutAPI)

type AuthAPI = LoginAPI :<|> LoginCallbackAPI :<|> LogoutAPI

login :: Vault -> Maybe String
    -> App (Headers '[Header "Location" String] NoContent)
login vault maybeReturnTo = do
    (_, sessionInsert) <- getSessionOrDie vault
    state <- liftIO $ do
        let returnTo = case maybeReturnTo of
                Just r -> r
                _      -> "/"
        sessionInsert "AUTH_RETURN_TO" $ strToBS returnTo
        state <- Spotify.generateState
        sessionInsert "SPOTIFY_AUTH_STATE" (strToBS $ show state)
        return state
    let base = showBaseUrl spotifyAccountsBaseUrl
        clientId = Just "foobar" :: Maybe String -- TODO: Use clientId from Config
        redirectUri = callbackLink
        scope = Spotify.scopeFromList $ Prelude.map pack
            ["user-read-email", "user-read-private", "playlist-read-private"]
        authLink = authorizeLink
            clientId
            (Just Spotify.ResponseType)
            (Just redirectUri)
            (Just state)
            (Just scope)
            Nothing
        location = base ++ "/" ++ show authLink
    return $ addHeader location NoContent

callback :: Vault -> Maybe Spotify.AuthorizationCode -> Maybe String -> Maybe Spotify.State
    -> App RedirectHeaders
callback vault maybeAuthCode maybeError maybeState =
    case maybeState of
        Nothing -> throwError $
            fromServantError $ badRequest "Auth state not provided"
        Just authState -> do
            session <- getSessionOrDie vault

            -- Check for matching auth state keys
            let (sessionLookup, sessionInsert) = session
            checkStatesOrDie authState sessionLookup

            case maybeAuthCode of
                Just authCode -> do
                    -- On Spotify auth success, request refresh and access
                    -- tokens from Spotify
                    cfg <- ask :: App Config
                    let redirectUri = Spotify.RedirectUri
                            (Just (pack callbackLink))
                        authTokenRequest = Spotify.makeAuthorizationCodeTokenRequest
                            authCode redirectUri

                    runTokenResponse <- liftIO $ runExceptT $ tokenRequest
                        authTokenRequest
                        (Spotify.Authorization "" "") -- TODO: Populate the authorization keys...
                        (getManager cfg)

                    case runTokenResponse of
                        Left e   -> throwError $ fromServantError $
                            serverError ("Could not retreive auth tokens: " ++ show e)
                        Right tokenResponse   -> do
                            let accessToken = Spotify.accessToken tokenResponse
                                refreshToken = Spotify.newRefreshToken tokenResponse

                            returnTo <- liftIO $ popAuthReturnOffSession session
                            return $ addHeader returnTo NoContent
                _ -> case maybeError of
                    Just spotifyError -> do
                        -- On Spotify auth error, flash the reason authorization
                        -- failed (e.g. "access_denied") and redirect to index
                        liftIO $
                            flash session $ M.singleton "error" spotifyError

                        returnTo <- liftIO $ popAuthReturnOffSession session
                        return $ addHeader returnTo NoContent
                    _ -> throwError $ fromServantError $
                        badRequest "Missing auth code or auth error"

checkStatesOrDie :: Spotify.State -> (Text -> IO (Maybe ByteString)) -> App ()
checkStatesOrDie state sessionLookup = do
    maybeSessionAuthState <- liftIO $ sessionLookup "SPOTIFY_AUTH_STATE"
    case maybeSessionAuthState of
        Just sessionAuthState ->
            unless (show state == bsToStr sessionAuthState) die
        Nothing -> die
    where
        die = throwError $ fromServantError $
            badRequest "Auth state mismatch"

popAuthReturnOffSession :: PartySession -> IO String
popAuthReturnOffSession session = popOffSession session "AUTH_RETURN_TO" "/"

-- https://developer.spotify.com/web-api/authorization-guide/#authorization_code_flow
-- https://github.com/spotify/web-api-auth-examples/blob/master/authorization_code/app.js#L58

logout :: Vault -> App RedirectHeaders
logout vault = do
    sessionState <- invalidateSession vault
    case sessionState of
        SessionInvalidated -> return $ addHeader "/" NoContent
        _                  -> throwError $ fromServantError noSessionError

authServer :: ServerT AuthAPI App
authServer = login :<|> callback :<|> logout
