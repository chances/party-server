{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spotify.Api.Types
    ( module Spotify.Api.Types
    , module Spotify.Api.Types.Scope
    ) where

import           Data.Aeson              (FromJSON (parseJSON), ToJSON,
                                          defaultOptions, genericParseJSON,
                                          genericToJSON, toJSON, withText)
import           Data.Aeson.Types        (Options (omitNothingFields))
import           Data.ByteString.Base64  (encode)
import qualified Data.ByteString.Char8   as C8
import           Data.Text               (Text, pack, unpack)
import           GHC.Generics            (Generic)
import           Servant                 (FromHttpApiData (..),
                                          ToFormUrlEncoded (..),
                                          ToHttpApiData (..), toQueryParam)
import           Test.RandomStrings      (onlyAlphaNum, randomASCII,
                                          randomString)

import           Spotify.Api.Types.Scope

data ResponseType = ResponseType

instance Show ResponseType where
    show _ = "code"

instance Read ResponseType where
    readsPrec _ str = wrap str (const ResponseType)

instance FromHttpApiData ResponseType where
    parseQueryParam _ = Right ResponseType

instance ToHttpApiData ResponseType where
    toQueryParam = pack . show

data State = State
    { getState :: String
    }

instance Show State where
    show = getState

instance Read State where
    readsPrec _ str =
        wrap str (\s -> State { getState = s })

instance FromHttpApiData State where
    parseQueryParam param = Right (read (unpack param) :: State)

instance ToHttpApiData State where
    toQueryParam = pack . show

wrap :: String -> (String -> a) -> [(a, String)]
wrap str convert = [(convert str, "")]

-- readState :: String -> [(State, String)]
-- readState str = [(State
--     { getState = fst . head . lex $ str
--     }, "")]

generateState :: IO State
generateState = do
    state <- randomString (onlyAlphaNum randomASCII) 16
    return State { getState = state }

data TokenRequest = TokenRequest
    { grantType    :: TokenGrantType
    , code         :: AuthorizationCode
    , redirectUri  :: RedirectUri
    , refreshToken :: RefreshToken
    } deriving (Generic)

newtype AuthorizationCode = AuthorizationCode (Maybe Text)
newtype RedirectUri = RedirectUri (Maybe Text)
newtype RefreshToken = RefreshToken (Maybe Text)

instance FromHttpApiData AuthorizationCode where
    parseQueryParam param = Right (AuthorizationCode (Just param))

instance ToHttpApiData AuthorizationCode where
    toQueryParam (AuthorizationCode maybeCode) = case maybeCode of
        Just authCode -> authCode
        _             -> ""

instance FromJSON RefreshToken where
    parseJSON = withText "RefreshToken" $ \value ->
        return $ RefreshToken (Just value)

instance ToJSON RefreshToken where
    toJSON (RefreshToken maybeToken) = toJSON maybeToken

instance ToFormUrlEncoded TokenRequest where
    toFormUrlEncoded tokenRequest = encodedTokenRequest where
        encodedTokenRequest = toFormUrlEncoded (grantType tokenRequest) ++
            toFormUrlEncoded (code tokenRequest) ++
            toFormUrlEncoded (redirectUri tokenRequest) ++
            toFormUrlEncoded (refreshToken tokenRequest)

data TokenGrantType =
    AuthorizationCodeGrantType
  | RefreshTokenGrantType

instance ToFormUrlEncoded TokenGrantType where
    toFormUrlEncoded tokenGrantType = case tokenGrantType of
        AuthorizationCodeGrantType -> [("grant_type", toQueryParam authCode)]
        RefreshTokenGrantType      -> [("grant_type", toQueryParam token)]
        where
            authCode = "authorization_code" :: String
            token = "refresh-token" :: String

instance ToFormUrlEncoded AuthorizationCode where
    toFormUrlEncoded (AuthorizationCode maybeCode) = case maybeCode of
        Just authCode -> [("code", toQueryParam authCode)]
        _             -> []

instance ToFormUrlEncoded RedirectUri where
    toFormUrlEncoded (RedirectUri maybeLink) = case maybeLink of
        Just link -> [("redirect_uri", toQueryParam link)]
        _         -> []

instance ToFormUrlEncoded RefreshToken where
    toFormUrlEncoded (RefreshToken maybeRefreshToken) = case maybeRefreshToken of
        Just token -> [("refresh_token", toQueryParam token)]
        _          -> []

makeAuthorizationCodeTokenRequest ::
    AuthorizationCode -> RedirectUri -> TokenRequest
makeAuthorizationCodeTokenRequest authCode redirect = TokenRequest
    { grantType = AuthorizationCodeGrantType
    , code = authCode
    , redirectUri = redirect
    , refreshToken = RefreshToken Nothing
    }

makeRefreshTokenRequest :: RefreshToken -> TokenRequest
makeRefreshTokenRequest token = TokenRequest
    { grantType = RefreshTokenGrantType
    , code = AuthorizationCode Nothing
    , redirectUri = RedirectUri Nothing
    , refreshToken = token
    }

data Authorization = Authorization
    { clientId        :: Text
    , clientSecretKey :: Text
    } deriving (Generic, Eq)

instance FromJSON Authorization
instance ToJSON Authorization

instance Show Authorization where
    show authorization = value where
        clientIdKey = unpack $ clientId authorization
        clientSecret = unpack $ clientSecretKey authorization
        keys = clientIdKey ++ ":" ++ clientSecret
        encodedKeys = encode $ C8.pack keys
        value = "Basic " ++ C8.unpack encodedKeys

instance ToHttpApiData Authorization where
    toQueryParam = pack . show

data TokenResponse = TokenResponse
    { accessToken     :: Text
    , tokenType       :: Bearer
    , scope           :: Scope
    , expiresIn       :: Int
    , newRefreshToken :: RefreshToken
    } deriving (Generic)

instance FromJSON TokenResponse where
    parseJSON = genericParseJSON doOmitNothingFields

instance ToJSON TokenResponse where
    toJSON = genericToJSON doOmitNothingFields

doOmitNothingFields :: Options
doOmitNothingFields = defaultOptions { omitNothingFields = True }

data Bearer = Bearer

instance Show Bearer where
    show _ = "Bearer"

instance Read Bearer where
    readsPrec _ str = wrap str (const Bearer)

instance FromJSON Bearer where
    parseJSON = withText "Bearer" $ \_ -> return Bearer

instance ToJSON Bearer where
    toJSON _ = toJSON $ show Bearer
