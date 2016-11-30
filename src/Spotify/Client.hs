module Spotify.Client
    ( AuthorizeApi
    , TokenApi
    , authorizeApi
    , authorizeLink
    , tokenApi
    , tokenRequest
    , spotifyAccountsBaseUrl
    , spotifyApiBaseUrl
    ) where

import           Servant.Client   (BaseUrl (..), Scheme (Https))

import           Spotify.Api.Auth

-- TODO: Document all public values in this client library
-- IDEA: Refactor this client library into it's own library for the masses

spotifyApiBaseUrl :: BaseUrl
spotifyApiBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.spotify.com"
    , baseUrlPort = 443
    , baseUrlPath = "v1"
    }
