module Handler.User (getIsUserR, getIsAdminR, getUsersR) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Database.Persist.Sql
import Yesod
import Yesod.Auth (maybeAuthId)

import Foundation
import Sugina.DB

getIsUserR :: Handler Value
getIsUserR = returnJson =<< runMaybeT (fmap serverUserName (MaybeT . runDB . get =<< MaybeT maybeAuthId))

getIsAdminR :: Handler Value
getIsAdminR = returnJson True  -- Already checked by `isAuthorized`

getUsersR :: Handler Value
getUsersR = returnJson =<< runDB (selectList ([] :: [Filter ServerUser]) [])
