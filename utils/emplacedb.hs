{-# LANGUAGE
    FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  , ViewPatterns
  #-}

-- Usage: Run by runhaskell.

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Kunyomi json
  wordEntity Text
  yomikata Text
  deriving Show
ServerUser
  credsPlugin Text
  credsIdent Text
  name Text
  isAdmin Bool
  deriving Show
|]

main :: IO ()
main = do
  (kunyomiWrd, yomikata) <- fmap (unzip . fmap (fmap T.tail . T.break (== '\t')) . T.lines) $ T.readFile "utils/kunyomi.txt"
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ zipWith Kunyomi kunyomiWrd yomikata
