{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserAPI
    , userServer
    ) where

import           Control.Monad.Reader        (liftIO)
import           Data.Int                    (Int64)
import           Data.List                   (sortOn)
import           Database.Persist.Postgresql (Entity (..), Key, SqlPersistT,
                                              fromSqlKey, insertUnique,
                                              selectFirst, selectList, (==.))
import           Servant

import           Config                      (App (..))
import           Database.Models
import           Database.Party              (runDb)
import           Utils                       (strToLazyBS)

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "user" :> Capture "id" UserId :> Get '[JSON] (Entity User)
    :<|> "user" :> ReqBody '[JSON] NewUser :> Post '[JSON] Int64

badRequest ::String -> ServantErr
badRequest message = err400 { errBody = strToLazyBS message }

notFound :: String -> ServantErr
notFound message = err404 { errBody = strToLazyBS message }

allUsers :: App [Entity User]
allUsers = runDb (selectList [] [])

singleUser :: UserId -> App (Entity User)
singleUser key = do
    maybeUser <- runDb (selectFirst [UserId ==. key] [])
    case maybeUser of
         Nothing ->
            throwError $ notFound ("User with Id " ++ show (fromSqlKey key) ++ " does not exist")
         Just person ->
            return person

createUser :: NewUser -> App Int64
createUser p = do
    user <- toUser p
    let insertUser = insertUnique user :: SqlPersistT IO (Maybe (Key User))
    maybeNewUserKey <- runDb insertUser
    case maybeNewUserKey of
        Nothing -> throwError $ badRequest "User was not created, ensure username is unique"
        Just newUserKey ->
            return $ fromSqlKey newUserKey

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser
