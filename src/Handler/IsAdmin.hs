module Handler.IsAdmin (getIsAdminR) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Yesod
import Yesod.Auth (maybeAuthId)

import DB
import Foundation

getIsAdminR :: Handler Value
getIsAdminR = returnJson =<< (runMaybeT $ fmap serverUserIsAdmin $ MaybeT . runDB . get =<< MaybeT maybeAuthId)
