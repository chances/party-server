{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

import           Control.Lens
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Int                   (Int64)
import           Data.Swagger
import           Data.Swagger.Declare
-- import           Data.Swagger.Lens
-- import           Data.Swagger.Operation
import           Data.Text                  (Text)
import           Database.Persist           (Entity (..))
-- import           Servant          ((:<|>), (:>), Proxy (..))
import           Servant                    ((:<|>), Proxy (..))
import           Servant.Swagger            (HasSwagger (..), toSwagger)
import           Web.Envelope               (Envelope' (..))
import qualified Web.Envelope               as E

-- import           Api.Auth         (AuthAPI)
-- import           Api.Envelope
import           Api.User                   (UserAPI, UserGetAPI, UserPostAPI,
                                             UsersGetAPI)
import           Database.Models

-- type AppAPI = ("auth" :> AuthAPI) :<|> UserAPI

-- instance (ToSchema e) => ToSchema (E.Err e) where
--     declareNamedSchema (E.Err e extra) =
--         genericDeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy e) <>
--             genericDeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy extra)

-- instance (ToSchema e) => ToSchema (E.Err e) where
--     declareNamedSchema (E.Err err extra) = do
--         errorTypeSchema <- declareSchemaRef (Proxy :: Proxy err)
--         extraTypeSchema <- declareSchemaRef (Proxy :: Proxy extra)
--         return $ NamedSchema (Just "Error") $ mempty
--             & type_ .~ SwaggerObject
--             & properties .~
--                 [ ("error", errorTypeSchema
--                     & description (Just "Error code or JSON encoded error"))
--                 , ("extra", extraTypeSchema
--                     & description (Just "Additional error information in plain text"))
--                 ]

-- instance (ToSchema (E.Err e), ToSchema (E.Success a)) => ToSchema (Envelope' (E.Err e) (E.Success a)) where
--     declareNamedSchema = genericDeclareNamedSchema schemaOptions

succeedWith :: ToSchema a => proxy a -> Text
    -> DeclareT (Definitions Schema) Identity NamedSchema
succeedWith proxy nameForSchema = do
    successTypeSchema <- declareSchemaRef proxy
    return $ NamedSchema (Just nameForSchema) $ mempty
        & type_ .~ SwaggerObject
        & properties .~
            [ ("data", successTypeSchema ) ]

succeedWithArray :: ToSchema a => proxy a -> Text
    -> DeclareT (Definitions Schema) Identity NamedSchema
succeedWithArray proxy nameForSchema = do
    successTypeSchema <- declareSchemaRef proxy
    return $ NamedSchema (Just nameForSchema) $ mempty
        & type_ .~ SwaggerObject
        & properties .~
            [ ("data", Inline $ mempty
                & items .~ Just (SwaggerItemsObject successTypeSchema)) ]

instance ToSchema (Envelope' (E.Err e) (E.Success (Entity User))) where
    declareNamedSchema _ = succeedWith (Proxy :: Proxy NewUser) "User Response"
instance ToSchema (Envelope' (E.Err e) (E.Success Int64)) where
    declareNamedSchema _ = succeedWith (Proxy :: Proxy Int64) "User ID"
instance ToSchema (Envelope' (E.Err e) (E.Success [Entity User])) where
    declareNamedSchema _ = succeedWithArray (Proxy :: Proxy [NewUser]) "Users"

-- instance ToSchema (Envelope' (E.Err e) (E.Success a)) where
--     declareNamedSchema _ = succeedWith (Proxy :: Proxy NewUser)
instance (ToSchema e) => ToSchema (E.Err e) where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions
instance (ToSchema a) => ToSchema (E.Success a) where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance ToSchema (Entity User) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy NewUser)
    -- declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy NewUser)
    --     & name .~ "User"
instance ToSchema NewUser where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions
instance ToParamSchema (Key User) where
    toParamSchema _ = toParamSchema (Proxy :: Proxy Int64)
        -- & description .~ "ID of user"

nameModifier :: String -> String
nameModifier "Envelope'" = "Response"
nameModifier "NewUser"   = "User"
nameModifier "Err"       = "Error"
nameModifier str         = str

schemaOptions :: SchemaOptions
schemaOptions = defaultSchemaOptions
    { datatypeNameModifier = nameModifier
    }

main :: IO ()
main = do
    -- Echo Swagger markup
    let userGets = Proxy :: Proxy (UserGetAPI :<|> UsersGetAPI)
        userPosts = Proxy :: Proxy UserPostAPI
    putStrLn $ show userGets ++ show userPosts
    putStrLn ""
    -- let getOps  = subOperations (Proxy :: Proxy (UserGetAPI :<|> UsersGetAPI)) (Proxy :: Proxy UserAPI)
    --     postOps = subOperations (Proxy :: Proxy UserPostAPI) (Proxy :: Proxy UserAPI)
    --     userApiSwagger = toSwagger (Proxy :: Proxy UserAPI)
    --         & applyTagsFor getOps  ["get"  & description ?~ "GET operations"]
    --         & applyTagsFor postOps ["post" & description ?~ "POST operations"]

    let userApiSwagger = toSwagger (Proxy :: Proxy UserAPI)

    BL8.putStr $ encode $ prependPath "api" userApiSwagger
        & info.title        .~ "Party API"
        & info.version      .~ "0.1"
        & info.description  ?~ "This is an API for the Users service"
        & info.license      ?~ "MIT"
        & host              ?~ "api.chancesnow.me"

    putStrLn "\n"
