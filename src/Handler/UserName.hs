module Handler.UserName (getUserNameR) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Yesod
import Yesod.Auth (maybeAuthId)

import DB
import Foundation

getUserNameR :: Handler Value
getUserNameR = returnJson =<< (runMaybeT $ fmap serverUserName $ MaybeT . runDB . get =<< MaybeT maybeAuthId)
