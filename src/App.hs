{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App (app, run) where

import           Control.Monad.Except
import           Control.Monad.Reader     (runReaderT)
import           Network.Wai              (Application, Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  ((:<|>) (..), (:~>) (Nat), Proxy (..),
                                           Raw, ServantErr, Server, enter,
                                           serve, serveDirectory)

import           Api.User                 (UserAPI, userServer)
import           Config                   (App (..), Config (..),
                                           envSetCorsOrigin, setLogger)
import           Database.Party           (runSqlPool)
import           Models

type AppAPI = UserAPI :<|> Raw

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg) userServer

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appAPI :: Proxy AppAPI
appAPI = Proxy

files :: Application
files = serveDirectory "assets"

app :: Config -> Application
app cfg = serve appAPI (appToServer cfg :<|> files)

run :: Config -> IO ()
run cfg = do
    let pool = getPool cfg
        port = getPort cfg
        -- Setup middleware
        env = getEnv cfg
        logger = setLogger env :: Middleware
        corsPolicy = envSetCorsOrigin env (getCorsOrigin cfg) :: Middleware

        application = logger $ corsPolicy $ app cfg

    -- Start Postgres pool and run the app
    runSqlPool doMigrations pool
    Warp.run port application
