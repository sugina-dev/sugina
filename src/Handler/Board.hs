{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handler.Board (getBoardManageR, getBoardMessageR, postBoardManageR, postBoardMessageR) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql
import GHC.Generics
import Yesod
import Yesod.Auth (maybeAuthId)

import DB
import Foundation

data GetBoardManageReturnValue = GetBoardManageReturnValue
  { msgid    :: !BoardId
  , time     :: !UTCTime
  , message  :: !Text
  , replymsg :: !(Maybe Text)
  , userName :: !Text
  } deriving (Generic, Show)

instance ToJSON GetBoardManageReturnValue

toGetBoardManageReturnValue :: (Single BoardId, Single UTCTime, Single Text, Single (Maybe Text), Single Text) -> GetBoardManageReturnValue
toGetBoardManageReturnValue (Single a,Single b,Single c,Single d,Single e) = GetBoardManageReturnValue a b c d e

data PostBoardManageRequestBody = PostBoardManageRequestBody
  { boardId :: !BoardId
  , reply   :: !Text
  } deriving (Generic, Show)

instance FromJSON PostBoardManageRequestBody

getBoardMessageR :: Handler Value
getBoardMessageR = do
  Just uploader <- maybeAuthId
  res <- runDB $ selectList [BoardUploader ==. uploader] []
  returnJson res

postBoardMessageR :: Handler Value
postBoardMessageR = do
  Just uploader <- maybeAuthId
  comment <- requireJsonBody :: Handler Text
  runDB $ rawExecute "INSERT INTO board (uploader, message) VALUES (?, ?)" [toPersistValue uploader, PersistText comment]
  returnJson True

getBoardManageR :: Handler Value
getBoardManageR = do
  res <- runDB $ rawSql "SELECT board.id, board.time, board.message, board.reply, server_user.name\
    \ FROM board JOIN server_user\
    \ ON board.uploader = server_user.id" [] :: Handler [(Single BoardId, Single UTCTime, Single Text, Single (Maybe Text), Single Text)]
  returnJson $ fmap toGetBoardManageReturnValue res

postBoardManageR :: Handler Value
postBoardManageR = do
  PostBoardManageRequestBody{boardId,reply} <- requireJsonBody
  runDB $ update boardId [BoardReply =. Just reply]
  returnJson True
