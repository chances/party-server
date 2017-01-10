module Main where

import qualified Configuration.Dotenv       as Dotenv
import           Control.Exception.Enclosed (catchAnyDeep)
import           System.Directory           (doesFileExist)
import           System.Environment         (lookupEnv)
import           System.Exit                (ExitCode (..), exitWith)
import           System.IO                  (BufferMode (LineBuffering),
                                             hSetBuffering, stdout)

import           App                        (run)
import           Config                     (Config (..),
                                             Environment (Development),
                                             defaultConfig, envManager,
                                             lookupSetting)
import           Database.Party             (makePool)

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
    corsOrigins <- lookupEnv "CORS_ORIGINS"
    pool <- makePool env
    manager <- envManager env

    let cfg = defaultConfig {
          getPool = pool
        , getPort = port
        , getEnv = env
        , getCorsOrigins = case corsOrigins of
            Nothing      -> "http://chancesnow.me"
            Just origins -> origins
        , getManager = manager
        }

    let errorHandler e = (do
            putStrLn $ "Uncaught exception: " ++ show e
            return $ ExitFailure (-1)) :: IO ExitCode
        runApp = do
            run cfg
            return ExitSuccess

    -- Run the app
    retCode <- catchAnyDeep runApp errorHandler

    exitWith retCode
