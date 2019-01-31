-- For emplacing the kunyomi data into the SQLite database.
-- This file is supposed to be run at the root of the project (i.e. `..`).

{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

import Data.Aeson (decodeFileStrict)
import qualified Data.Map.Strict as M (keys)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist
import Database.Persist.Sqlite

import Sugina.DB
import Sugina.Secret

main :: IO ()
main = do
  -- Kunyomi
  lns <- fmap T.lines $ T.readFile "utils/kunyomi.txt"
  let (kunyomiWrd, yomikata) = unzip $ fmap ((\[l,r] -> (l, r)) . T.split (== '\t')) lns
  -- ServerUser
  Just Secret{getHardcodedUsers} <- decodeFileStrict ".secret.json"
  let unames = M.keys getHardcodedUsers
  -- Migrate
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ zipWith Kunyomi kunyomiWrd yomikata
    insertMany_ $ fmap (\uname -> ServerUser "hardcoded" uname uname True) unames
