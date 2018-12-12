{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Foundation where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M (member)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite
import Yesod
import Yesod.Auth
import Yesod.Auth.Hardcoded
import Yesod.Auth.OAuth2.GitLab
import qualified Yesod.Auth.Message as Msg
import Yesod.Static (Static)

import DB
import Enc

data App = App
  { getPubdyn :: Static
  , getPubsta :: Static
  , getPool   :: ConnectionPool
  , getEnc    :: Enc
  }

mkYesodData "App" [parseRoutesNoCheck|
/ RootR GET
/auth AuthR Auth getAuth

/api/username UserNameR GET

/api/dictum DictumR GET
/api/kunyomi/#Text KunyomiR GET

/pubdyn PubdynR Static getPubdyn
/ PubstaR Static getPubsta
|]

instance Yesod App where
  approot = ApprootMaster $ \App{getEnc} -> let Enc{getApproot} = getEnc in getApproot
  isAuthorized RootR         False = pure Authorized
  isAuthorized (AuthR _    ) _     = pure Authorized
  isAuthorized UserNameR     False = pure Authorized
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
  authPlugins App{getEnc} = let Enc{getGitLabClientId,getGitLabClientSecret} = getEnc in
    [ authHardcoded
    , oauth2GitLab getGitLabClientId getGitLabClientSecret
    ]
  authenticate c@Creds{credsPlugin} =
    case credsPlugin of
      "hardcoded" -> authenticateHardcoded c
      "gitlab"    -> authenticateGitLab c
      _           -> pure $ UserError Msg.InvalidLogin
  loginDest _ = RootR
  logoutDest _ = RootR
  onLogin = pure ()

authenticateHardcoded :: (MonadHandler m, HandlerSite m ~ App) => Creds master -> m (AuthenticationResult App)
authenticateHardcoded Creds{credsIdent} = fmap Authenticated
  $ liftHandler
  $ runDB
  $ fmap (\[x] -> x)
  $ selectKeysList [ServerUserCredsPlugin ==. "hardcoded", ServerUserCredsIdent ==. credsIdent] []

authenticateGitLab :: (MonadHandler m, HandlerSite m ~ App) => Creds master -> m (AuthenticationResult App)
authenticateGitLab _ = pure undefined

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = runSqlPool action . getPool =<< getYesod

instance YesodAuthPersist App where
  type AuthEntity App = ServerUser

instance YesodAuthHardcoded App where
  doesUserNameExist uname = do
    hu <- fmap (getHardcodedUsers . getEnc) getYesod
    pure $ uname `M.member` hu
  validatePassword uname pwd = do
    hu <- fmap (getHardcodedUsers . getEnc) getYesod
    pure $ hu !? uname == Just pwd
