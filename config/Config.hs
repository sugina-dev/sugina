-- For emplacing the server user data into the SQLite database.
-- This file is supposed to be run at the root of the project (i.e. `..`).

{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import Data.Aeson (decodeFileStrict)
import qualified Data.Map.Strict as M (keys)
import Database.Persist
import Database.Persist.Sqlite

import Sugina.DB
import Sugina.Secret

main :: IO ()
main = do
  -- ServerUser
  Just Secret{getHardcodedUsers} <- decodeFileStrict ".secret.json"
  let unames = M.keys getHardcodedUsers
  -- Migrate
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ fmap (\uname -> ServerUser "hardcoded" uname uname True) unames
