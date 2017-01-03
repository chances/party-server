{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App (app, run) where

import           Control.Monad.Except
import           Control.Monad.Reader     (runReaderT)
import           Data.Text                (unpack)
import           Network.Wai              (Application, Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  ((:<|>) (..), (:>), (:~>) (Nat),
                                           Proxy (..), Raw, ServantErr, Server,
                                           ServerT, enter, layout, serve,
                                           serveDirectory)

import           Api.Auth                 (AuthAPI, authServer)
import           Api.User                 (UserAPI, userServer)
import           Config                   (App (runApp), Config (..),
                                           Environment (Development),
                                           envSetCorsOrigin, setLogger)
import           Database.Party           (doMigrations, runSqlPool)
import           Middleware.Flash         (flashMiddleware)
import           Middleware.Session       (sessionMiddleware)

type AppAPI = ("auth" :> AuthAPI) :<|> UserAPI

type AppWithFiles = AppAPI :<|> Raw

appServer :: ServerT AppAPI App
appServer = authServer :<|> userServer

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server AppAPI
appToServer cfg = enter (convertApp cfg) appServer

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appAPI :: Proxy AppWithFiles
appAPI = Proxy

files :: Application
files = serveDirectory "public"

app :: Config -> Application
app cfg = serve appAPI (appToServer cfg :<|> files)

run :: Config -> IO ()
run cfg = do
    let port = getPort cfg
        pool = getPool cfg
        -- Setup middleware
        env = getEnv cfg
        logger = setLogger env :: Middleware
        corsPolicy = envSetCorsOrigin env (getCorsOrigin cfg) :: Middleware
        flash = flashMiddleware cfg :: Middleware

    session <- sessionMiddleware cfg

    -- Print API layout
    case env of
        Development -> putStrLn $ unpack $ layout appAPI
        _           -> putStr ""

    -- Compose middleware pipeline
    let middleware = logger . corsPolicy . session . flash
        application = middleware $ app cfg

    -- Start Postgres pool and run the app
    runSqlPool doMigrations pool
    Warp.run port application
