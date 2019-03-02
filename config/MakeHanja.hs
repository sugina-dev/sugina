-- For emplacing the hanja data into the SQLite database.
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
  -- Hanja
  hanjaLns <- fmap T.lines $ T.readFile "config/hanja.txt"
  let (char, yomikata) = unzip $ fmap ((\[l,r] -> (l, r)) . T.words) hanjaLns
  -- Migrate
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ zipWith Hanja char yomikata
