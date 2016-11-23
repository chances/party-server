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
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), Key, SqlPersistT,
                                              fromSqlKey, insertUnique,
                                              selectFirst, selectList, (==.))
import           Servant

import           Config                      (App (..))
import           Models
import           Utils                       (strToLazyBS)

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "user" :> Capture "id" UserId :> Get '[JSON] (Entity User)
    :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] Int64

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

newUser :: String -> String -> App User
newUser username spotifyUser = do
    currentTime <- liftIO getCurrentTime
    return (User username spotifyUser Nothing Nothing (Just currentTime) (Just currentTime))

createUser :: User -> App Int64
createUser p = do
    user <- newUser (userUsername p) (userSpotifyUser p)
    let insertUser = insertUnique user :: SqlPersistT IO (Maybe (Key User))
    maybeNewUserKey <- runDb insertUser
    case maybeNewUserKey of
        Nothing -> throwError $ badRequest "User was not created, ensure username is unique"
        Just newUserKey ->
            return $ fromSqlKey newUserKey

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser
