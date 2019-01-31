{-# LANGUAGE DeriveGeneric #-}

module Sugina.Secret where

import Data.Aeson (FromJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data Secret = Secret
  { getHardcodedUsers      :: !(Map Text Text)  --  Hard-coded users that exists primitively
  , getGitLabClientId      :: !Text  -- For GitLab login support
  , getGitLabClientSecret  :: !Text  -- For GitLab login support
  , getApproot             :: !Text  -- Without the trailing `/`
  } deriving (Generic, Show)

instance FromJSON Secret
