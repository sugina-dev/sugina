{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (decodeFileStrict)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip
import Yesod
import Yesod.Auth (getAuth)
import Yesod.Static (static, staticDevel)

import DB
import Foundation
import Handler.Dictum (getDictumR)
import Handler.Kunyomi (getKunyomiR)
import Handler.Root (getRootR)
import Handler.UserName (getUserNameR)
import Secret

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  -- Handling secret
  Just secret@Secret{getPridynPath,getPristaPath} <- decodeFileStrict ".secret.json"
  -- Run migration
  runSqlite "data.db" $ runMigration migrateAll
  -- Start server
  runStderrLoggingT
    $ withSqlitePool "data.db" 10  -- openConnectionCount
    $ \pool -> liftIO $ do
      pridyn <- staticDevel getPridynPath  -- Private, Dynamic
      prista <- static      getPristaPath
      pubdyn <- staticDevel "pubdyn"       -- Public, Dynamic
      pubsta <- static      "pubsta"       -- Public, Static
      wApp <- toWaiApp App
        { getSecret = secret  -- For start-up configurations
        , getPridyn = pridyn  -- For static files
        , getPrista = prista  -- For static files
        , getPubdyn = pubdyn  -- For static files
        , getPubsta = pubsta  -- For static files
        , getPool   = pool    -- For Database
        }
      run 80 $ gzip def{gzipFiles = GzipCompress} $ wApp
