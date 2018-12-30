{-# LANGUAGE AutoDeriveTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module DB where

import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Kunyomi json
  wordEntity Text
  yomikata Text
  deriving Show
ServerUser
  credsPlugin Text
  credsIdent Text
  name Text
  isAdmin Bool
  UniqueServerUser credsPlugin credsIdent
  deriving Show
Board json
  time UTCTime "default=(datetime('now'))"
  uploader ServerUserId
  message Text
  reply Text Maybe default=NULL
  deriving Show
|]
