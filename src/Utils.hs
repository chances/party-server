module Utils
    ( badRequest
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

badRequest :: String -> ServantErr
badRequest message = err400 { errBody = strToLazyBS message }

notFound :: String -> ServantErr
notFound message = err404 { errBody = strToLazyBS message }

serverError :: String -> ServantErr
serverError message = err500 { errBody = strToLazyBS message }

strToBS :: String -> C8.ByteString
strToBS = C8.pack

strToLazyBS :: String -> LazyC8.ByteString
strToLazyBS = LazyC8.pack
