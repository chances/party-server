module Main where

import qualified Configuration.Dotenv     as Dotenv
import           Network.Wai.Handler.Warp (run)
import           System.Directory         (doesFileExist)
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

    -- Load the .env configuration file, if present
    dotenvExists <- doesFileExist ".env"
    if dotenvExists
        then Dotenv.loadFile False ".env"
        else do
            putStrLn "Warning: .env configuration file does not exist"
            putStrLn
                (
                   "\tEnsure proper configuration exists in the environment"
                ++ "(See .env.example)"
                )

    -- Setup app configuration
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8080
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getPort = port, getEnv = env }
        logger = setLogger env

    -- Start Postgres pool and run the app
    runSqlPool doMigrations pool
    run port $ logger $ app cfg
