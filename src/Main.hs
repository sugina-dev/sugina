{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (decodeFileStrict)
import Database.Persist.Sqlite (runMigration, runSqlite, withSqlitePool)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles(GzipCompress), def, gzip, gzipFiles)
import Yesod
import Yesod.Auth (getAuth)

import Foundation
import Handler.Board (getBoardManageR, getBoardMessageR, postBoardManageR, postBoardMessageR)
import Handler.Dictum (getDictumR)
import Handler.Kanji (getHanjaR, getKunyomiR)
import Handler.Root (getRootR)
import Handler.User (getIsUserR, getIsAdminR, getUsersR)
import Sugina.DB

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  -- Handling secret
  Just secret <- decodeFileStrict ".secret.json"
  -- Run migration
  runSqlite "data.db" $ runMigration migrateAll
  -- Start server
  runNoLoggingT
    $ withSqlitePool "data.db" 20  -- openConnectionCount
    $ \pool -> liftIO $ do
      wApp <- toWaiApp App
        { getSecret = secret  -- For start-up configurations
        , getPool   = pool    -- For Database
        }
      run 3000
        $ gzip def{gzipFiles = GzipCompress}
        $ wApp
