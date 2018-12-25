module Handler.Home (getHomeR) where

import Data.Text (Text)
import Yesod
import Yesod.Auth (maybeAuthId)

import DB
import Foundation

getHomeR :: Handler ()
getHomeR = maybeAuthId >>= \case
  Nothing  -> redirect ("pubdyn/index.html" :: Text)
  Just aid -> do
    ent <- runDB $ getJust aid
    if serverUserIsAdmin ent
      then redirect ("pridyn/index.html" :: Text)
      else redirect ("pubdyn/index.html" :: Text)
