{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserAPI
    , userServer
    ) where

import           Data.Int                    (Int64)
import           Data.List                   (sortOn)
import           Database.Persist.Postgresql (Entity (..), Key, SqlPersistT,
                                              fromSqlKey, insertUnique,
                                              selectFirst, selectList, (==.))
import           Servant

import           Api.Envelope                (Envelope, fromServantError,
                                              success)
import           Config                      (App (..))
import           Database.Models
import           Database.Party              (runDb)
import           Middleware.Session          (SessionState (..),
                                              invalidateSession, startSession)
import           Utils                       (badRequest, notFound, serverError)

type UserAPI =
         "users" :> Get '[JSON] (Envelope [Entity User])
    :<|> "user" :> Capture "id" UserId :> Get '[JSON] (Envelope (Entity User))
    :<|> "user" :> Vault :> ReqBody '[JSON] NewUser :> Post '[JSON] (Envelope Int64)
    :<|> "logout" :> Vault :> Get '[JSON] (Envelope String)

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

createUser :: Vault -> NewUser -> App (Envelope Int64)
createUser vault p = do
    user <- toUser p
    let insertUser = insertUnique user :: SqlPersistT IO (Maybe (Key User))
    maybeNewUserKey <- runDb insertUser
    case maybeNewUserKey of
        Nothing -> throwError $ fromServantError $ badRequest "User was not created, ensure username is unique"
        Just newUserKey -> do
            sessionState <- startSession vault (fromSqlKey newUserKey)
            case sessionState of
                SessionStarted _ -> return $ success $ fromSqlKey newUserKey
                _                -> throwError $ fromServantError noSessionError

logout :: Vault -> App (Envelope String)
logout vault = do
    sessionState <- invalidateSession vault
    case sessionState of
        SessionInvalidated -> return $ success "Logout"
        _                  -> throwError $ fromServantError noSessionError

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser :<|> logout
