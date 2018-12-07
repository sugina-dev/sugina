{-# LANGUAGE
    DeriveGeneric
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  , ViewPatterns
  #-}

-- Usage: Run by runhaskell.

import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (keys)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)

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

data Enc = Enc
  { getHardcodedUsers :: Map Text Text
  } deriving (Generic, Show)

instance FromJSON Enc

main :: IO ()
main = do
  -- Kunyomi
  lns <- fmap T.lines $ T.readFile "utils/kunyomi.txt"
  let (kunyomiWrd, yomikata) = unzip $ fmap ((\[l,r] -> (l, r)) . T.split (== '\t')) lns
  -- ServerUser
  Just Enc{getHardcodedUsers} <- decodeFileStrict ".enc"
  let unames = M.keys getHardcodedUsers
  -- Migrate
  runSqlite "data.db" $ do
    runMigration migrateAll
    insertMany_ $ zipWith Kunyomi kunyomiWrd yomikata
    insertMany_ $ fmap (\uname -> ServerUser "hardcoded" uname uname True) unames
