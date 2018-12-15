module Handler.Home (getHomeR) where

import Data.Text (Text)
import Yesod
import Yesod.Auth (maybeAuthId)

import Foundation

getHomeR :: Handler ()
getHomeR = maybeAuthId >>= \case
  Nothing -> redirect ("pubdyn/index.html" :: Text)
  Just _  -> redirect ("pridyn/index.html" :: Text)
