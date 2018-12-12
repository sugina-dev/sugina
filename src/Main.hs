{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (decodeFileStrict)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (run)
import Yesod
import Yesod.Auth (getAuth)
import Yesod.Static (static, staticDevel)

import DB
import Foundation
import Handler.Dictum (getDictumR)
import Handler.Kunyomi (getKunyomiR)
import Handler.Root (getRootR)
import Handler.UserName (getUserNameR)

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  -- Handling secret
  Just secret <- decodeFileStrict ".secret.json"
  -- Run migration
  runSqlite "data.db" $ runMigration migrateAll
  -- Start server
  runStderrLoggingT
    $ withSqlitePool "data.db" 10  -- openConnectionCount
    $ \pool -> liftIO $ do
      pubdyn <- staticDevel "pubdyn"  -- Public, Dynamic
      pubsta <- static      "pubsta"  -- Public, Static
      wApp <- toWaiApp App
        { getPubdyn = pubdyn  -- For static files
        , getPubsta = pubsta  -- For static files
        , getPool   = pool    -- For Database
        , getSecret = secret  -- For Hard-coded Auth Routine
        }
      run 80 wApp
