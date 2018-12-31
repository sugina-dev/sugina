module Handler.Users (getUsersR) where

import Database.Persist.Sql
import Yesod

import DB
import Foundation

getUsersR :: Handler Value
getUsersR = returnJson =<< (runDB $ selectList ([] :: [Filter ServerUser]) [])
