module Utils
    ( strToBS
    , strToLazyBS
    ) where

import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LazyC8

strToBS :: String -> C8.ByteString
strToBS = C8.pack

strToLazyBS :: String -> LazyC8.ByteString
strToLazyBS = LazyC8.pack
