module Main where

import qualified Configuration.Dotenv as Dotenv
import           System.Directory     (doesFileExist)
import           System.IO            (BufferMode (LineBuffering),
                                       hSetBuffering, stdout)

import           App                  (run)
import           Config               (Config (..), Environment (Development),
                                       defaultConfig, envManager, lookupSetting)
import           Database.Party       (makePool)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- Load the .env configuration file, if present
    dotenvExists <- doesFileExist ".env"
    if dotenvExists
        then Dotenv.loadFile False ".env"
        else do
            putStrLn "Warning: '.env' configuration file does not exist!\n"
            putStrLn
                (  "\tApplication may not function as intended.\n"
                ++ "\tEnsure proper configuration exists in the environment.\n\n"
                ++ "\t(See .env.example)"
                )

    -- Setup app configuration
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8080
    corsOrigin <- lookupSetting "CORS_ORIGIN" "http://chancesnow.me"
    pool <- makePool env
    manager <- envManager env

    let cfg = defaultConfig {
          getPool = pool
        , getPort = port
        , getEnv = env
        , getCorsOrigin = corsOrigin
        , getManager = manager
        }

    run cfg
