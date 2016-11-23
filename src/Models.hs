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

import           Control.Monad.Reader        (MonadIO, MonadReader, asks,
                                              liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Time                   (UTCTime (..))
import           Database.Persist.Postgresql (SqlPersistT, runMigration,
                                              runSqlPool)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           GHC.Generics                (Generic)

import           Config                      (Config (..), getPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    username String
    spotifyUser String
    accessToken String Maybe
    refreshToken String Maybe

    createdAt UTCTime Maybe default="(now() at time zone 'utc')"
    updatedAt UTCTime Maybe default="(now() at time zone 'utc')"

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
