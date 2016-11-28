{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserAPI
    , userServer
    ) where

import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), Key, SqlPersistT,
                                              fromSqlKey, insertUnique,
                                              selectFirst, selectList, (==.))
import           Servant
import           Servant.Utils.Links         (safeLink)

import           Api.Envelope                (Envelope, fromServantError,
                                              success)
import           Config                      (App (..))
import           Database.Models
import           Database.Party              (runDb)
import           Middleware.Session          (SessionState (..),
                                              invalidateSession, startSession)
import           Utils                       (badRequest, notFound, serverError)

type UsersGetAPI = "users"
    :> Get '[JSON] (Envelope [Entity User])
type UserGetAPI  = "user"
    :> Capture "id" UserId
    :> Get '[JSON] (Envelope (Entity User))
type UserPostAPI = "user"
    :> Vault :> ReqBody '[JSON] NewUser
    :> PostCreated '[JSON] (Headers '[Header "Location" String] (Envelope Int64))

type LogoutAPI   = "logout"
    :> Vault
    :> Get '[JSON] (Envelope String)

type UserAPI =
         UsersGetAPI :<|> UserGetAPI :<|> UserPostAPI
    :<|> LogoutAPI

noSessionError :: ServantErr
noSessionError = serverError "No session"

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
            sessionState <- startSession vault key
            case sessionState of
                SessionStarted _ -> do
                    let newUserLink = safeLink (Proxy :: Proxy UserAPI) (Proxy :: Proxy UserGetAPI)
                        resource = success key
                        response = addHeader (show $ newUserLink newUserKey) resource
                    return response
                _                -> throwError $ fromServantError noSessionError

logout :: Vault -> App (Envelope String)
logout vault = do
    sessionState <- invalidateSession vault
    case sessionState of
        SessionInvalidated -> return $ success "Logout"
        _                  -> throwError $ fromServantError noSessionError

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser :<|> logout
