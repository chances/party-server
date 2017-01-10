{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.PingResponse
    ( PingResponse(..)
    , ApiStatus(..)
    , apiAvailable
    ) where

import           Data.Aeson       (FromJSON, ToJSON, object, parseJSON, toJSON,
                                   withObject, (.:), (.=))
import           Data.Aeson.Types (Parser)
import           GHC.Generics     (Generic)

data PingResponse = PingResponse
    { api_status :: ApiStatus
    , message    :: String
    } deriving (Generic)

instance FromJSON PingResponse
instance ToJSON PingResponse

apiAvailable :: String -> PingResponse
apiAvailable = PingResponse Available

data ApiStatus =
    Available
  | Unavailable

instance FromJSON ApiStatus where
  parseJSON = withObject "ApiStatus" $ \o -> do
      parsedAvailable <- o .: "available" :: Parser Bool
      return $ case parsedAvailable of
          True  -> Available
          False -> Unavailable

instance ToJSON ApiStatus where
  toJSON status = toJSON $ object
    [ "available" .= case status of
        Available   -> True
        Unavailable -> False
    ]
