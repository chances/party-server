{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Spotify.Api.Auth
    ( AuthorizeApi
    , TokenApi
    , authorizeApi
    , authorizeLink
    , tokenApi
    , tokenRequest
    , spotifyAccountsBaseUrl
    ) where

import           Network.HTTP.Client (Manager)
import           Servant
import           Servant.Client      (BaseUrl (..), ClientM, Scheme (Https),
                                      client)

import           Spotify.Api.Types

type AuthorizeApi = "authorize"
    :> QueryParam "client_id" String
    :> QueryParam "response_type" ResponseType
    :> QueryParam "redirect_uri" String
    :> QueryParam "state" State
    :> QueryParam "scope" Scope
    :> QueryParam "show_dialog" Bool
    :> Get '[PlainText] String

authorizeApi :: Proxy AuthorizeApi
authorizeApi = Proxy

authorizeLink :: HasLink AuthorizeApi => MkLink AuthorizeApi
authorizeLink = safeLink authorizeApi authorizeApi

type TokenApi = "token"
    :> ReqBody '[FormUrlEncoded] TokenRequest
    :> Header "Authorization" Authorization
    :> Post '[JSON] TokenResponse

tokenApi :: Proxy TokenApi
tokenApi = Proxy

spotifyAccountsBaseUrl :: BaseUrl
spotifyAccountsBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "accounts.spotify.com"
    , baseUrlPort = 443
    , baseUrlPath = "/api"
    }

tokenRequest ::
    TokenRequest -> Authorization -> Manager -> ClientM TokenResponse
tokenRequest token auth manager =
    let tokenApiClient = client tokenApi
    in tokenApiClient token (Just auth) manager spotifyAccountsBaseUrl
