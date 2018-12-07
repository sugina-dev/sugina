{-# LANGUAGE DeriveGeneric #-}

module Enc where

import Data.Aeson (FromJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data Enc = Enc
  { getHardcodedUsers :: Map Text Text
  } deriving (Generic, Show)

instance FromJSON Enc
