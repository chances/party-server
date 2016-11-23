{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App (app, run) where

import           Control.Monad.Except
import           Control.Monad.Reader       (MonadIO, MonadReader, ReaderT,
                                             runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Either (EitherT)
import           Network.Wai                (Application, Middleware)
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant                    ((:<|>) (..), (:>), (:~>) (Nat),
                                             Proxy (..), Raw, ServantErr,
                                             Server, enter, serve,
                                             serveDirectory)

import           Api.User                   (UserAPI, userServer)
import           Config                     (App (..), Config (..), setLogger)
import           Database.Party             (runSqlPool)
import           Models

type AppM = ReaderT Config (EitherT ServantErr IO)

type AppAPI = UserAPI :<|> Raw

userApp :: Config -> Application
userApp cfg = serve (Proxy :: Proxy UserAPI) (appToServer cfg)

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
    let logger = setLogger (getEnv cfg) :: Middleware

    -- Start Postgres pool and run the app
    let pool = getPool cfg
        port = getPort cfg
    runSqlPool doMigrations pool
    Warp.run port $ logger $ app cfg
