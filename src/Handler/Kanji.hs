{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Kanji (getHanjaR, getKunyomiR) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Text (Text)
import Database.Persist
import Yesod

import Foundation
import Sugina.DB

queryOneKunyomi :: Text -> Handler [Text]
queryOneKunyomi wrd
  = runDB
  $ fmap (fmap (kunyomiYomikata . entityVal))
  $ selectList [KunyomiChar ==. wrd] []

queryOneHanja :: Text -> Handler [Text]
queryOneHanja wrd
  = runDB
  $ fmap (fmap (hanjaYomikata . entityVal))
  $ selectList [HanjaChar ==. wrd] []

getKunyomiR :: Handler Value
getKunyomiR = returnJson =<< runMaybeT (MaybeT . fmap (fmap pure) queryOneKunyomi =<< MaybeT (lookupGetParam "q"))

getHanjaR :: Handler Value
getHanjaR = returnJson =<< runMaybeT (MaybeT . fmap (fmap pure) queryOneHanja =<< MaybeT (lookupGetParam "q"))
