{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserAPI
    , UserGetAPI
    , UserPostAPI
    , UsersGetAPI
    , getUserLink
    , getUsersLink
    , postUserLink
    , userServer
    ) where

import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Reader        (ask, liftIO)
import           Data.Aeson                  (decode)
import           Data.Int                    (Int64)
import           Data.Text                   (Text, pack)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), Key, SqlPersistT,
                                              fromSqlKey, insertUnique,
                                              selectFirst, selectList, (==.))
import           Database.Persist.Postgresql as Persist
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html             (Html)
import qualified Network.Spotify.Api.Types.User as Spotify

import           Api.Auth                    (Redirect, RedirectHeaders)
import           Api.Envelope                (Envelope, fromServantError,
                                              success)
import           Config                      (App (..), Config (getManager))
import           Database.Models
import           Database.Party              (runDb)
import           Middleware.Session          (SessionState (..), startSession,
                                              getUserFromSession, getUsernameFromSession)
import           Middleware.Flash            (getFlashedError)
import           Network.Spotify                (getMePlaylists)
import qualified Network.Spotify                as Spotify
import qualified Network.Spotify.Api.Types.User as Spotify.User
import qualified Network.Spotify.Api.Types.Paging as Spotify.Paging
import           Network.Spotify.Api.Types.PlaylistSimplified as Playlist
import           Utils                       (badRequest, forbidden, noSessionError,
                                              notFound, strToLazyBS)
import           Views.Index                 (render)

type Index = Vault :> Get '[HTML] Html

type Playlist = "playlist"
    :> Vault
    :> QueryParam "id" String
    :> QueryParam "return_to" String
    :> Redirect

type UsersGetAPI = "users"
    :> Get '[JSON] (Envelope [Entity User])
type UserGetAPI  = "user"
    :> Capture "id" UserId
    :> Get '[JSON] (Envelope (Entity User))
type UserPostAPI = "user"
    :> Vault :> ReqBody '[JSON] NewUser
    :> PostCreated '[JSON] (Headers '[Header "Location" String] (Envelope Int64))

getUsersLink :: (IsElem UsersGetAPI UserAPI, HasLink UsersGetAPI) => MkLink UsersGetAPI
getUsersLink = safeLink (Proxy :: Proxy UserAPI) (Proxy :: Proxy UsersGetAPI)

getUserLink :: (IsElem UserGetAPI UserAPI, HasLink UserGetAPI) => MkLink UserGetAPI
getUserLink = safeLink (Proxy :: Proxy UserAPI) (Proxy :: Proxy UserGetAPI)

postUserLink :: (IsElem UserPostAPI UserAPI, HasLink UserPostAPI) => MkLink UserPostAPI
postUserLink = safeLink (Proxy :: Proxy UserAPI) (Proxy :: Proxy UserPostAPI)

type UserAPI =
        Index :<|>
        Playlist :<|>
        UsersGetAPI :<|> UserGetAPI :<|> UserPostAPI

-- | Index route controller
index :: Vault -> App Html
index vault = do
    cfg <- ask :: App Config

    maybeError <- getFlashedError vault

    -- Try to get current user
    eitherUser <- getUserFromSession vault
    let
            ( maybeUser
                , maybeAccessToken
                , maybeRefreshToken
                , maybeCurrentPlaylistId
                ) =
                case eitherUser of
                    Left _ -> (Nothing, Nothing, Nothing, Nothing)
                    Right sessionUser ->
                        ( decode $ strToLazyBS (userSpotifyUser sessionUser)
                        , userAccessToken sessionUser
                        , userRefreshToken sessionUser
                        , userSpotifyPlaylistId sessionUser
                        )
                        :: ( Maybe Spotify.User, Maybe String, Maybe String, Maybe String)

    -- Try to get current user's playlists
    eitherPlaylists <- getPlaylists cfg maybeAccessToken

    -- Render the view
    case eitherPlaylists of
        Left Nothing    -> return $ render
            maybeUser
            Nothing -- No playlists
            maybeCurrentPlaylistId
            maybeError
        Left (Just err) -> return $ render
            maybeUser Nothing maybeCurrentPlaylistId
            (case maybeError of -- Concatenate errors if already exists
                Nothing -> Just $ show err
                Just e -> Just (e ++ "\n\n" ++ show err))
        Right playlists -> return $ render
            maybeUser
            (case maybeUser of
                Just user -> Just $ ownPlaylists (Spotify.User.id user) playlists
                Nothing -> Nothing)
            maybeCurrentPlaylistId
            maybeError

-- | Playlist route controller
playlist :: Vault -> Maybe String -> Maybe String -> App RedirectHeaders
playlist vault maybePlaylistId maybeReturnTo = do
    let returnTo = case maybeReturnTo of
            Just r -> r
            _      -> "/"
        response = addHeader returnTo NoContent

    -- Try to get current username
    maybeUsername <- getUsernameFromSession vault

    case maybeUsername of
        Just user -> do
            currentTime <- liftIO getCurrentTime
            let updateUser = Persist.updateWhere [UserUsername ==. user]
                    [ UserSpotifyPlaylistId =. maybePlaylistId
                    , UserUpdatedAt =. currentTime
                    ]
                    :: SqlPersistT IO ()
                    in runDb updateUser
            return response
        Nothing -> throwError $ fromServantError $
            forbidden "Session is unauthenticated"

getPlaylists :: Config -> Maybe String ->
    App (Either (Maybe String) [Playlist.PlaylistSimplified])
getPlaylists cfg maybeAccessToken = case maybeAccessToken of
        Just token -> do
            getMePlaylistsResponse <- liftIO $ runExceptT $
                getMePlaylists
                    (Spotify.Authorization $ pack token)
                    (Spotify.Paging.firstPage 50) Nothing
                    (getManager cfg)

            case getMePlaylistsResponse of
                Left e -> return $ Left (Just $ show e)
                Right playlists -> return $ Right
                    (Spotify.Paging.items playlists
                        :: [Playlist.PlaylistSimplified])
        Nothing -> return $ Left Nothing

ownPlaylists :: Text -> [Playlist.PlaylistSimplified] ->
    [Playlist.PlaylistSimplified]
ownPlaylists userId = filter (\p ->
        Spotify.User.id (Playlist.owner p) == userId
    )

allUsers :: App (Envelope [Entity User])
allUsers = do
    users <- runDb (selectList [] [])
    return $ success users

singleUser :: UserId -> App (Envelope (Entity User))
singleUser key = do
    maybeUser <- runDb (selectFirst [UserId ==. key] [])
    case maybeUser of
         Nothing ->
            throwError $ fromServantError $ notFound ("User with Id " ++ show (fromSqlKey key) ++ " does not exist")
         Just person ->
            return $ success person

createUser :: Vault -> NewUser -> App (Headers '[Header "Location" String] (Envelope Int64))
createUser vault p = do
    user <- toUser p
    let insertUser = insertUnique user :: SqlPersistT IO (Maybe (Key User))
    maybeNewUserKey <- runDb insertUser
    case maybeNewUserKey of
        Nothing -> throwError $ fromServantError $ badRequest "User was not created, ensure username is unique"
        Just newUserKey -> do
            let key = fromSqlKey newUserKey
            sessionState <- startSession vault (username p)
            case sessionState of
                SessionStarted _ -> do
                    let resource = success key
                        response = addHeader (show $ getUserLink newUserKey) resource
                    return response
                _                -> throwError $ fromServantError noSessionError

userServer :: ServerT UserAPI App
userServer = index :<|> playlist :<|> allUsers :<|> singleUser :<|> createUser
