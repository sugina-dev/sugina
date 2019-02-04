module Handler.User (getIsUserR, getIsAdminR, getUsersR) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Database.Persist.Sql
import Yesod
import Yesod.Auth (maybeAuthId)

import Foundation
import Sugina.DB

getIsUserR :: Handler Text
getIsUserR = fmap fromJust $ runMaybeT (fmap serverUserName (MaybeT . runDB . get =<< MaybeT maybeAuthId))

getIsAdminR :: Handler ()
getIsAdminR = pure ()  -- Already checked by `isAuthorized`

getUsersR :: Handler Value
getUsersR = returnJson =<< runDB (selectList ([] :: [Filter ServerUser]) [])
