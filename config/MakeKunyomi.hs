-- For emplacing the kunyomi data into the SQLite database.
-- This file is supposed to be run at the root of the project (i.e. `..`).

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist
import Database.Persist.Sqlite

import Sugina.DB

main :: IO ()
main = do
  -- Kunyomi
  kunLns <- fmap T.lines $ T.readFile "config/kunyomi.txt"
  let (kunChar, kunYomikata) = unzip $ fmap ((\[l,r] -> (l, r)) . T.words) kunLns
  -- Migrate
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ zipWith Kunyomi kunChar kunYomikata
