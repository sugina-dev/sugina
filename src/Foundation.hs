{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Foundation where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite
import Yesod
import Yesod.Auth
import Yesod.Auth.Hardcoded
import Yesod.Static (Static)

import DB

data App = App
  { getPubdyn        :: Static
  , getPubsta        :: Static
  , getPool          :: ConnectionPool
  , getAdminPassword :: Text
  }

mkYesodData "App" [parseRoutesNoCheck|
/ RootR GET
/auth AuthR Auth getAuth

/api/dictum DictumR GET
/api/kunyomi/#Text KunyomiR GET

/pubdyn PubdynR Static getPubdyn
/ PubstaR Static getPubsta
|]

instance Yesod App where
  isAuthorized RootR         False = pure Authorized
  isAuthorized (AuthR _    ) _     = pure Authorized
  isAuthorized DictumR       False = pure Authorized
  isAuthorized (KunyomiR _)  False = pure Authorized
  isAuthorized (PubdynR _)   False = pure Authorized
  isAuthorized (PubstaR _)   False = pure Authorized
  isAuthorized _             _     = checkAdminById =<< maybeAuthId

checkAdminById :: Maybe ServerUserId -> Handler AuthResult
checkAdminById Nothing    = pure AuthenticationRequired
checkAdminById (Just aid) = do
  ent <- runDB $ getJust aid
  pure $ if serverUserIsAdmin ent
    then Authorized
    else Unauthorized "You should be an administrator!"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = ServerUserId
  authPlugins _ = [authHardcoded]
  authenticate _ = pure $ Authenticated $ toSqlKey 1
  loginDest _ = RootR
  logoutDest _ = RootR
  onLogin = pure ()

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = runSqlPool action . getPool =<< getYesod

instance YesodAuthPersist App where
  type AuthEntity App = ServerUser

instance YesodAuthHardcoded App where
  doesUserNameExist usr    = pure $ usr == "Admin"
  validatePassword usr pwd = do
    adminpwd <- fmap (getAdminPassword) getYesod
    pure $ usr == "Admin" && pwd == adminpwd
