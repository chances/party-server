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

module Database.Models where

import           Control.Monad.Reader        (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Time                   (UTCTime (..))
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql as Persist
import           Database.Persist.TH         (mkPersist, mkSave,
                                              persistLowerCase, share,
                                              sqlSettings)
import           GHC.Generics                (Generic)

import           Config                      (App (..))

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
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
            (accessToken newUser) (refreshToken newUser)
            currentTime currentTime
        )

touchUser :: Persist.Key User -> SqlPersistT IO ()
touchUser userKey = do
    currentTime <- liftIO getCurrentTime
    Persist.update userKey
       [ UserUpdatedAt =. currentTime
       ]
