module Main where

import           Network.Wai.Handler.Warp (run)
import           System.IO                (BufferMode (LineBuffering), IO (..),
                                           hSetBuffering, stdout)

import           App                      (app)
import           Config                   (Config (..),
                                           Environment (Development),
                                           defaultConfig, envPool,
                                           lookupSetting, setLogger)
import           Database.Party           (makePool, runSqlPool)
import           Models                   (doMigrations)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- Setup app configuration
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8080
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getPort = port, getEnv = env }
        logger = setLogger env

    -- Start Postgres pool and run the app
    runSqlPool doMigrations pool
    run port $ logger $ app cfg
