module Data.Enc (getEnc) where

import Data.Map.Strict as M
import qualified Text.Regex.PCRE as R ((=~))

(=~) :: String -> String -> [[String]]
(=~) = (R.=~)

getEnc :: IO (Map String String)
getEnc = do
  cont <- readFile ".enc"
  let pairs = cont =~ "^([^:]+):(.+)$"
  pure $ M.fromList $ fmap (\[_,l,r] -> (l,r)) pairs
