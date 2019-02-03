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
  kunLns <- fmap T.lines $ T.readFile "util/kunyomi.txt"
  let (kunChar, kunYomikata) = unzip $ fmap ((\[l,r] -> (l, r)) . T.words) kunLns
  -- Hanja
  hanjaLns <- fmap T.lines $ T.readFile "util/hanja.txt"
  let (hanjaChar, hanjaYomikata) = unzip $ fmap ((\[l,r] -> (l, r)) . T.words) hanjaLns
  -- ServerUser
  Just Secret{getHardcodedUsers} <- decodeFileStrict ".secret.json"
  let unames = M.keys getHardcodedUsers
  -- Migrate
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ zipWith Kunyomi kunChar kunYomikata
    insertMany_ $ zipWith Hanja hanjaChar hanjaYomikata
    insertMany_ $ fmap (\uname -> ServerUser "hardcoded" uname uname True) unames
