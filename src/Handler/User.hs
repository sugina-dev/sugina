module Handler.User (getIsUserR, getIsAdminR, getUsersR) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Yesod
import Yesod.Auth (maybeAuthId)

import Foundation
import Sugina.DB

getIsUserR :: Handler Text
getIsUserR = do
  Just aid <- maybeAuthId
  runDB
    $ fmap (serverUserName . fromJust)
    $ get aid

getIsAdminR :: Handler ()
getIsAdminR = pure ()  -- Already checked by `isAuthorized`

getUsersR :: Handler Value
getUsersR = returnJson =<< runDB (selectList ([] :: [Filter ServerUser]) [])
