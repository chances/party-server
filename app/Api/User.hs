{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserAPI(..)
    , userServer
    ) where

import           Control.Monad.Reader        (liftIO)
import qualified Data.ByteString.Lazy        as BS
import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as C8
import           Data.Int                    (Int64)
import           Data.List                   (sortOn)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Class      (liftPersist)
import           Database.Persist.Postgresql (Entity (..), Key,
                                              PersistEntityBackend, SqlPersistT,
                                              fromSqlKey, insertUnique,
                                              selectFirst, selectList, (==.))
import           Servant

import           Config                      (App (..))
import           Models

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "user" :> Capture "id" UserId :> Get '[JSON] (Entity User)
    :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] Int64

strToBS :: String -> ByteString
strToBS = C8.pack

notFound :: String -> ServantErr
notFound message = err404 { errBody = strToBS message }

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
    return (User username spotifyUser Nothing Nothing currentTime currentTime)

createUser :: User -> App Int64
createUser p = do
    user <- newUser (userUsername p) (userSpotifyUser p)
    let insertUser = insertUnique user :: SqlPersistT IO (Maybe (Key User))
    maybeNewUserKey <- runDb insertUser
    case maybeNewUserKey of
        Nothing -> throwError err400
        Just newUserKey ->
            return $ fromSqlKey newUserKey

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser
