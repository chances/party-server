{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Database.Persist.Postgresql (SqlPersistT, runMigration,
                                              runSqlPool)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           Control.Monad.Reader (liftIO)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Time            (UTCTime (..))
import           Data.Time.Clock      (getCurrentTime)
import           GHC.Generics         (Generic)

import           Config               (App (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    username String
    spotifyUser String
    accessToken String Maybe
    refreshToken String Maybe

    createdAt UTCTime
    updatedAt UTCTime

    UniqueUsername username
    deriving Eq
    deriving Show
    deriving Generic
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
data NewUser = NewUser
    { username     :: String
    , spotifyUser  :: String
    , accessToken  :: Maybe String
    , refreshToken :: Maybe String
    } deriving (Show, Generic)

instance ToJSON NewUser
instance FromJSON NewUser

toUser :: NewUser -> App User
toUser newUser = do
    currentTime <- liftIO getCurrentTime
    return (
        User (username newUser) (spotifyUser newUser)
            Nothing Nothing
            currentTime currentTime
        )
