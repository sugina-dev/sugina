{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Kunyomi (getKunyomiR) where

import Data.Text (Text)
import Database.Persist
import Yesod

import DB
import Foundation

queryOne :: Text -> Handler [Text]
queryOne wrd = runDB
  $ fmap (fmap (kunyomiYomikata . entityVal))
  $ selectList [KunyomiWordEntity ==. wrd] []

getKunyomiR :: Text -> Handler Value
getKunyomiR wrd = returnJson =<< queryOne wrd
