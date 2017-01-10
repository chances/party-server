{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Auth
    ( AuthAPI
    , Redirect
    , RedirectHeaders
    , authServer
    ) where

import           Control.Monad                  (unless)
import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Reader           (ask, liftIO)
import           Data.Aeson                     (FromJSON, ToJSON, encode)
import           Data.ByteString                (ByteString)
import           Data.Map                       as M
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text, pack, unpack)
import           Data.Time.Clock                (UTCTime, addUTCTime,
                                                 getCurrentTime)
import           Database.Persist.Postgresql    (Entity (..), Key, SqlPersistT,
                                                 getBy, insertUnique, (=.))
import           Database.Persist.Postgresql    as Persist
import           GHC.Generics                   (Generic)
import           Servant
import           Servant.Common.BaseUrl         (showBaseUrl)

import           Api.Envelope                   (Envelope, fromServantError,
                                                 success)
import           Api.Types.PingResponse         (PingResponse, apiAvailable)
import           Config                         (App (..), Config (getManager, getSpotifyAuthorization, getSpotifyCallback),
                                                 PartySession)
import           Database.Models
import           Database.Party                 (runDb)
import           Middleware.Flash               (flash)
import           Middleware.Session             (SessionState (SessionInvalidated),
                                                 getSessionOrDie,
                                                 getUserFromSession,
                                                 invalidateSession,
                                                 popOffSession, startSession)
import           Network.Spotify                (authorizeLink, getMe,
                                                 spotifyAccountsBaseUrl,
                                                 tokenRequest)
import qualified Network.Spotify                as Spotify
import qualified Network.Spotify.Api.Types.User as Spotify.User
import           Utils                          (badRequest, bsToStr,
                                                 lazyBsToStr, noSessionError,
                                                 serverError, strToBS,
                                                 unauthorized)

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
    :> Redirect
type LoginCallbackAPI = "callback"
    :> Vault
    :> QueryParam "code" Spotify.AuthorizationCode
    :> QueryParam "error" String
    :> QueryParam "state" Spotify.State
    :> Redirect

type PingAPI = "ping" :> Vault :> Get '[JSON] (Envelope PingResponse)

type TokenAPI = "token" :> Vault :> Get '[JSON] TokenResponse

type LogoutAPI   = "logout"
    :> Vault :> Redirect

getLoginLink :: HasLink LoginAPI => MkLink LoginAPI
getLoginLink = safeLink (Proxy :: Proxy AuthAPI) (Proxy :: Proxy LoginAPI)

instance HasLink sub => HasLink (Vault :> sub) where
    type MkLink (Vault :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)

getCallbackLink :: HasLink LoginCallbackAPI => MkLink LoginCallbackAPI
getCallbackLink = safeLink (Proxy :: Proxy AuthAPI) (Proxy :: Proxy LoginCallbackAPI)

getLogoutLink :: HasLink LogoutAPI => MkLink LogoutAPI
getLogoutLink = safeLink (Proxy :: Proxy AuthAPI) (Proxy :: Proxy LogoutAPI)

type AuthAPI = LoginAPI :<|> LoginCallbackAPI :<|> PingAPI :<|> TokenAPI :<|> LogoutAPI

login :: Vault -> Maybe String -> App RedirectHeaders
login vault maybeReturnTo = do
    (_, sessionInsert) <- getSessionOrDie vault
    cfg <- ask :: App Config
    state <- liftIO $ do
        let returnTo = case maybeReturnTo of
                Just r -> r
                _      -> "/"
        sessionInsert "AUTH_RETURN_TO" $ strToBS returnTo
        state <- Spotify.generateState
        sessionInsert "SPOTIFY_AUTH_STATE" (strToBS $ show state)
        return state
    let base = showBaseUrl spotifyAccountsBaseUrl
        clientId = Just $
            unpack $ Spotify.clientId $ getSpotifyAuthorization cfg
        redirectUri = getSpotifyCallback cfg
        requiredScope = Spotify.scopeFromList $ Prelude.map pack
            [ "user-read-email"
            , "user-read-private"
            , "playlist-read-private"
            , "playlist-read-collaborative"
            ]
        authLink = authorizeLink
            clientId
            (Just Spotify.ResponseType)
            (Just redirectUri)
            (Just state)
            (Just requiredScope)
            Nothing
        location = base ++ "/" ++ show authLink
    return $ addHeader location NoContent

-- TODO: Refactor this auth callback monstrosity...
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
            -- Done with auth state, pop it off the session
            _ <- liftIO $ popOffSession session "SPOTIFY_AUTH_STATE" ""

            returnTo <- liftIO $ popAuthReturnOffSession session
            let response = addHeader returnTo NoContent

            case maybeAuthCode of
                Just authCode -> do
                    -- On Spotify auth success, request refresh and access
                    -- tokens from Spotify
                    cfg <- ask :: App Config
                    let redirectUri = Spotify.RedirectUri
                            (Just $ pack (getSpotifyCallback cfg))
                        authTokenRequest = Spotify.makeAuthorizationCodeTokenRequest
                            authCode redirectUri

                    runTokenResponse <- liftIO $ runExceptT $ tokenRequest
                        authTokenRequest
                        (getSpotifyAuthorization cfg)
                        (getManager cfg)

                    case runTokenResponse of
                        Left e   -> throwError $ fromServantError $
                            serverError ("Could not retreive auth tokens: " ++ show e)
                        Right tokenResponse   -> do
                            let newAccessToken = Spotify.access_token tokenResponse
                                newRefreshToken = Spotify.refresh_token tokenResponse

                            -- Calculate token exiry date
                            -- TODO: Modularize this calulation and unit test
                            currentTime <- liftIO getCurrentTime
                            let tokenExpiry = addUTCTime
                                    (realToFrac $ Spotify.expires_in tokenResponse)
                                    currentTime

                            getMeResponse <- liftIO $ runExceptT $ getMe
                                (Spotify.Authorization newAccessToken)
                                (getManager cfg)

                            case getMeResponse of
                                Left e -> throwError $ fromServantError $
                                    serverError ("Could not retreive user: " ++ show e)
                                Right user -> do
                                    let userId = unpack $ Spotify.User.id user
                                        newUserAccessToken = Just $ unpack newAccessToken
                                        newUserRefreshToken = case newRefreshToken of
                                            Spotify.RefreshToken Nothing -> Nothing
                                            Spotify.RefreshToken (Just t) -> Just $ unpack t
                                        newUserTokenScope = Just $ show (Spotify.scope tokenResponse)
                                        newUser = NewUser
                                            userId
                                            (lazyBsToStr $ encode user)
                                            newUserAccessToken
                                            newUserRefreshToken
                                            (Just tokenExpiry)
                                            newUserTokenScope
                                        getUserByUsername = getBy $ UniqueUsername userId
                                            :: SqlPersistT IO (Maybe (Entity User))
                                    maybeUser <- runDb getUserByUsername
                                    case maybeUser of
                                         Nothing -> do
                                             userToInsert <- toUser newUser
                                             let insertUser = insertUnique userToInsert :: SqlPersistT IO (Maybe (Key User))
                                             maybeNewUserKey <- runDb insertUser
                                             case maybeNewUserKey of
                                                 Nothing -> throwError $
                                                     fromServantError $
                                                     serverError "User was not created at login"
                                                 Just _ -> do
                                                    _ <- startSession vault userId
                                                    return response

                                         Just (Entity userKey _) -> do
                                             let updateUser = Persist.update userKey
                                                    [ UserSpotifyUser =. spotifyUser newUser
                                                    , UserAccessToken =. accessToken newUser
                                                    , UserRefreshToken =. refreshToken newUser
                                                    , UserTokenExpiryDate =. tokenExpiryDate newUser
                                                    , UserTokenScope =. tokenScope newUser
                                                    , UserUpdatedAt =. currentTime
                                                    ]
                                                    :: SqlPersistT IO ()
                                             runDb updateUser
                                             _ <- startSession vault userId
                                             return response
                _ -> case maybeError of
                    Just spotifyError -> do
                        -- On Spotify auth error, flash the reason authorization
                        -- failed (e.g. "access_denied") and redirect to index
                        liftIO $
                            flash session $ M.singleton "error" spotifyError

                        return response
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

ping :: Vault -> App (Envelope PingResponse)
ping _ = do
    -- TODO: Verify Origin header for CORS, and what else to protect endpoint?
    -- REVIEW: When is the API not available?

    return $ success $ apiAvailable "Party API is available"

token :: Vault -> App TokenResponse
token vault = do
    eitherUser <- getUserFromSession vault

    -- TODO: Check expiry of token, if expired then refresh it

    case eitherUser of
        Left _ -> throwError $ fromServantError (unauthorized "Session is unauthenticated")
        Right sessionUser -> return tokenResponse where
            tokenResponse = TokenResponse
                (pack $ fromJust $ userAccessToken sessionUser)
                (fromJust $ userTokenExpiryDate sessionUser)
                (pack $ fromJust $ userTokenScope sessionUser)

data TokenResponse = TokenResponse
    { access_token :: Text
    , expiry_date  :: UTCTime
    , scope        :: Text
    } deriving (Generic)

instance FromJSON TokenResponse
instance ToJSON TokenResponse

logout :: Vault -> App RedirectHeaders
logout vault = do
    sessionState <- invalidateSession vault
    case sessionState of
        SessionInvalidated -> return $ addHeader "/" NoContent
        _                  -> throwError $ fromServantError noSessionError

authServer :: ServerT AuthAPI App
authServer = login :<|> callback :<|> ping :<|> token :<|> logout
