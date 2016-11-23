{-# LANGUAGE OverloadedStrings #-}

module Database.Party
    ( makePool
    , Postgres.runSqlPool
    ) where

import           Config                      (Environment (..), envPool)

import           Control.Exception           (throwIO)
import           Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import           Data.ByteString.Char8       (pack)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              createPostgresqlPool)
import qualified Database.Persist.Postgresql as Postgres
import           System.Environment          (lookupEnv)

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
