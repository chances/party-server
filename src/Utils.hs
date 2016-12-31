module Utils
    ( badRequest
    , baseUrl
    , bsToStr
    , lazyBsToStr
    , noSessionError
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
import           Servant.Client             (BaseUrl (..), Scheme (Http))

-- TODO: Make this configurable via environment (move to Config?)

baseUrl :: BaseUrl
baseUrl = BaseUrl
    { baseUrlScheme = Http
    , baseUrlHost = "localhost"
    , baseUrlPort = 3000
    , baseUrlPath = ""
    }

jsonContentType :: HTTP.Header
jsonContentType = (HTTP.hContentType, strToBS "application/json")

noSessionError :: ServantErr
noSessionError = serverError "No session"

badRequest :: String -> ServantErr
badRequest message = err400 { errBody = strToLazyBS message }

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
