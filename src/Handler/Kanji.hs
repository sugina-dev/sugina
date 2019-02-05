{-# LANGUAGE DeriveGeneric #-}

module Handler.Kanji (getHanjaR, getKunyomiR) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (length)
import Database.Esqueleto (SqlBackend, (^.), from, like, select, val, where_)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (badRequest400)
import Yesod

import Foundation
import Sugina.DB

data YomikataGroup = YomikataGroup
  { char     :: !Text
  , yomikata :: !Text
  } deriving (Generic, Show)

instance ToJSON YomikataGroup

getQuery :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => EntityField a Text -> (a -> YomikataGroup) -> Maybe Text -> Handler Value
getQuery ef cy mch = if isInvalid
  then sendResponseStatus badRequest400 ()
  else returnJson =<< queryOne (fromJust mch)
 where
  isInvalid :: Bool
  isInvalid = mch == Nothing || fmap T.length mch /= Just 1 || mch == Just "%"
  queryOne :: Text -> Handler [YomikataGroup]
  queryOne ch
    = runDB
    $ fmap (fmap (cy . entityVal))
    $ select
    $ from
    $ \k -> do
      where_ $ k ^. ef `like` val ("%" <> ch <> "%")
      pure k

getKunyomiR :: Handler Value
getKunyomiR = getQuery KunyomiChar (YomikataGroup <$> kunyomiChar <*> kunyomiYomikata) =<< lookupGetParam "q"

getHanjaR :: Handler Value
getHanjaR = getQuery HanjaChar (YomikataGroup <$> hanjaChar <*> hanjaYomikata) =<< lookupGetParam "q"
