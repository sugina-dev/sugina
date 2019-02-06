{-# LANGUAGE DeriveGeneric  #-}

module Handler.Board (getBoardManageR, getBoardMessageR, postBoardManageR, postBoardMessageR) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Esqueleto (InnerJoin(..), (^.), from, isNothing, on, select, set, updateCount, val, where_)
import qualified Database.Esqueleto as E ((=.), (==.))
import Database.Persist.Sql ((==.), rawExecute)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (badRequest400)
import Yesod
import Yesod.Auth (maybeAuthId)

import Foundation
import Sugina.DB

data GetBoardManageReturnValue = GetBoardManageReturnValue
  { msgid    :: !BoardId
  , time     :: !UTCTime
  , message  :: !Text
  , replymsg :: !(Maybe Text)
  , userName :: !Text
  } deriving (Generic, Show)

instance ToJSON GetBoardManageReturnValue

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

postBoardMessageR :: Handler ()
postBoardMessageR = do
  Just uploader <- maybeAuthId
  comment <- requireJsonBody :: Handler Text
  runDB $ rawExecute "INSERT INTO board (uploader, message) VALUES (?, ?)" [toPersistValue uploader, PersistText comment]

getBoardManageR :: Handler Value
getBoardManageR = returnJson =<< query
 where
  query :: Handler [GetBoardManageReturnValue]
  query = runDB
    $ fmap (fmap makeRet)
    $ select
    $ from
    $ \(b `InnerJoin` s) -> do
      on (b ^. BoardUploader E.==. s ^. ServerUserId)
      pure (b, s)
  makeRet :: (Entity Board, Entity ServerUser) -> GetBoardManageReturnValue
  makeRet (b,s) = GetBoardManageReturnValue
    { msgid    = entityKey b
    , time     = boardTime $ entityVal b
    , message  = boardMessage $ entityVal b
    , replymsg = boardReply $ entityVal b
    , userName = serverUserName $ entityVal s
    }

postBoardManageR :: Handler ()
postBoardManageR = do
  PostBoardManageRequestBody{boardId,reply} <- requireJsonBody
  n <- runDB $ updateCount $ \m -> do
    set m [BoardReply E.=. val (Just reply)]
    where_ $ m ^. BoardId E.==. val boardId
    where_ $ isNothing $ m ^. BoardReply
  if n == 0
    then sendResponseStatus badRequest400 ()
    else pure ()
