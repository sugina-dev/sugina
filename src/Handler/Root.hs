module Handler.Root (getRootR) where

import Data.Text (Text)
import Yesod

import Foundation

getRootR :: Handler ()
getRootR = redirect ("https://sugina.cc" :: Text)
