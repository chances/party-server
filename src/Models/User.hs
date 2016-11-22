{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.User (User(..)) where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registrationDate :: Day
} deriving (Show, Eq, Generic)

instance FromJSON User
instance ToJSON User
