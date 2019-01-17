{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (decodeFileStrict)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import Network.Wai.Middleware.Gzip
import Yesod
import Yesod.Auth (getAuth)
import Yesod.Static (staticDevel)

import DB
import Foundation
import Handler.Board (getBoardManageR, getBoardMessageR, postBoardManageR, postBoardMessageR)
import Handler.Dictum (getDictumR)
import Handler.IsAdmin (getIsAdminR)
import Handler.Kunyomi (getKunyomiR)
import Handler.Root (getRootR)
import Handler.Users (getUsersR)
import Handler.UserName (getUserNameR)
import Secret

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  -- Handling secret
  Just secret@Secret{getPridynPath,getCertificateFilePath,getKeyFilePath} <- decodeFileStrict ".secret.json"
  -- Run migration
  runSqlite "data.db" $ runMigration migrateAll
  -- Start server
  runStderrLoggingT
    $ withSqlitePool "data.db" 10  -- openConnectionCount
    $ \pool -> liftIO $ do
      pridyn <- staticDevel getPridynPath  -- Private, Dynamic
      wApp <- toWaiApp App
        { getSecret = secret  -- For start-up configurations
        , getPridyn = pridyn  -- For static files
        , getPool   = pool    -- For Database
        }
      runTLS (tlsSettings getCertificateFilePath getKeyFilePath) (setPort 3000 defaultSettings)
        $ gzip def{gzipFiles = GzipCompress}
        $ wApp
