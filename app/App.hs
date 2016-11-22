{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App (run) where

import Models.User

import Data.List
import Data.Time.Calendar (fromGregorian)

import Servant
import Web.HttpApiData (parseBoundedTextData)
import qualified Network.Wai.Handler.Warp as Warp

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name | Email | Registration deriving (Show, Bounded, Enum)

instance FromHttpApiData SortBy where
    parseUrlPiece = parseBoundedTextData
    parseQueryParam = parseBoundedTextData

sortedBy :: Ord b => [a] -> (a -> b) -> [a]
sortedBy list projector = sortOn projector list

users :: [User]
users =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

server :: Server UserAPI
server = users'
    where users' :: Maybe SortBy -> Handler [User]
          users' sortKey = case sortKey of
            Nothing -> return users
            Just Age -> return $ users `sortedBy` age
            Just Name -> return $ users `sortedBy` name
            Just Email -> return $ users `sortedBy` email
            Just Registration -> return $ users `sortedBy` registrationDate

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

run :: Warp.Port -> IO ()
run port = Warp.run port app
