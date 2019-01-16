{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Foundation where

import Control.Monad.Fail (MonadFail, fail)
import Data.Aeson ((.:), decode)
import Data.Aeson.Types (parseMaybe)
import Data.Foldable (find)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M (member)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)
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
import Secret
import Sugina.AuthHardcoded (authHardcodedAlter)

data App = App
  { getSecret :: !Secret
  , getPridyn :: !Static
  , getPrista :: !Static
  , getPool   :: !ConnectionPool
  }

mkYesodData "App" [parseRoutesNoCheck|
/ RootR GET
/auth AuthR Auth getAuth

/api/username UserNameR GET
/api/isadmin IsAdminR GET
/api/users UsersR GET

/api/dictum DictumR GET
/api/kunyomi/#Text KunyomiR GET
/api/board/message BoardMessageR GET POST
/api/board/manage BoardManageR GET POST

/pridyn PridynR Static getPridyn
/prista PristaR Static getPrista
|]

instance MonadFail (HandlerFor App) where
  fail = undefined

instance Yesod App where
  approot = ApprootMaster $ \App{getSecret} -> let Secret{getApproot} = getSecret in getApproot
  isAuthorized RootR         False = pure Authorized
  isAuthorized (AuthR _ )    _     = pure Authorized
  isAuthorized UserNameR     False = pure Authorized
  isAuthorized IsAdminR      False = pure Authorized
  isAuthorized UsersR        False = checkAdminById =<< maybeAuthId
  isAuthorized DictumR       False = pure Authorized
  isAuthorized (KunyomiR _)  False = pure Authorized
  isAuthorized BoardMessageR _     = fmap checkLoggedInById maybeAuthId
  isAuthorized BoardManageR  _     = checkAdminById =<< maybeAuthId
  isAuthorized (PridynR _)   False = checkAdminById =<< maybeAuthId
  isAuthorized (PristaR _)   False = checkAdminById =<< maybeAuthId
  isAuthorized _             _     = checkAdminById =<< maybeAuthId

checkLoggedInById :: Maybe ServerUserId -> AuthResult
checkLoggedInById Nothing  = AuthenticationRequired
checkLoggedInById (Just _) = Authorized  

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
  authPlugins App{getSecret} = let Secret{getGitLabClientId,getGitLabClientSecret} = getSecret in
    [ authHardcodedAlter
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
authenticateHardcoded Creds{credsIdent}
  = fmap Authenticated
  $ liftHandler
  $ runDB
  $ fmap (\[x] -> x)
  $ selectKeysList [ServerUserCredsPlugin ==. "hardcoded", ServerUserCredsIdent ==. credsIdent] []

authenticateGitLab :: (MonadHandler m, HandlerSite m ~ App) => Creds master -> m (AuthenticationResult App)
authenticateGitLab Creds{credsIdent,credsExtra}
  = fmap Authenticated
  $ liftHandler
  $ runDB
  $ fmap entityKey
  $ upsert (ServerUser "gitlab" credsIdent (getGitLabUserName credsExtra) False) []

-- | Get GitLab user name from credsExtra
getGitLabUserName :: [(Text, Text)] -> Text
getGitLabUserName
  = fromJust
  . parseMaybe (.: "name")
  . fromJust
  . decode
  . TL.encodeUtf8
  . TL.fromStrict
  . snd
  . fromJust
  . find (\p -> fst p == "userResponse")

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = runSqlPool action . getPool =<< getYesod

instance YesodAuthPersist App where
  type AuthEntity App = ServerUser

instance YesodAuthHardcoded App where
  doesUserNameExist uname = do
    hu <- fmap (getHardcodedUsers . getSecret) getYesod
    pure $ uname `M.member` hu
  validatePassword uname pwd = do
    hu <- fmap (getHardcodedUsers . getSecret) getYesod
    pure $ hu !? uname == Just pwd
