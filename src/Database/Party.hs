{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell            #-}

module Database.Party
    ( doMigrations
    , makePool
    , Postgres.runSqlPool
    , runDb
    ) where

import           Control.Exception           (throwIO)
import           Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader        (MonadIO, MonadReader, asks,
                                              liftIO)
import qualified Data.Proxy          as P
import           Data.ByteString.Char8       (pack)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString, SqlPersistT,
                                              createPostgresqlPool, runMigration, runSqlPool)
import qualified Database.Persist.Postgresql as Postgres
import Database.Persist.TH (mkMigrate)
import           System.Environment          (lookupEnv)
import qualified Web.ServerSession.Backend.Persistent as SS
import qualified Web.ServerSession.Core               as SS

import           Config                      (Config (..), Environment (..), envPool, getPool)
import Database.Models (entityDefs)

makePool :: Environment -> IO ConnectionPool
makePool Test = do
    connectionString <- connStr
    runNoLoggingT $ createPostgresqlPool connectionString (envPool Test)
makePool env = do
    connectionString <- connStr
    runStdoutLoggingT $ createPostgresqlPool connectionString (envPool env)

connStr :: IO ConnectionString
connStr = do
    maybeDbUrl <- lookupEnv "DATABASE_URL"
    case maybeDbUrl of
        Nothing -> throwIO (userError "Database Configuration not present in environment.")
        Just dbUrl -> return $ pack dbUrl

mkMigrate "migrateAll" (SS.serverSessionDefs (P.Proxy :: P.Proxy SS.SessionMap) ++ entityDefs)

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
