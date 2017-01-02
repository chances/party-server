{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( badRequest
    , bsToStr
    , lazyBsToStr
    , noSessionError
    , unauthorized
    , forbidden
    , notFound
    , serverError
    , jsonContentType
    , strToBS
    , strToLazyBS
    ) where

import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LazyC8
import           Network.HTTP.Types         as HTTP
import           Servant

jsonContentType :: HTTP.Header
jsonContentType = (HTTP.hContentType, strToBS "application/json")

noSessionError :: ServantErr
noSessionError = serverError "No session"

badRequest :: String -> ServantErr
badRequest message = err400 { errBody = strToLazyBS message }

unauthorized :: String -> ServantErr
unauthorized message = err401
    { errHeaders = [("Www-Authenticate", strToBS $
        "Bearer realm=\"spotify\", error=\"unauthorized\", "
        ++ "error_description=\"" ++ message ++ "\"")]
    , errBody = strToLazyBS message
    }

forbidden :: String -> ServantErr
forbidden message = err403 { errBody = strToLazyBS message }

notFound :: String -> ServantErr
notFound message = err404 { errBody = strToLazyBS message }

serverError :: String -> ServantErr
serverError message = err500 { errBody = strToLazyBS message }

strToBS :: String -> C8.ByteString
strToBS = C8.pack

bsToStr :: C8.ByteString -> String
bsToStr = C8.unpack

lazyBsToStr :: LazyC8.ByteString -> String
lazyBsToStr = LazyC8.unpack

strToLazyBS :: String -> LazyC8.ByteString
strToLazyBS = LazyC8.pack
