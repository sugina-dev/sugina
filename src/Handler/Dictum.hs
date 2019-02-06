module Handler.Dictum (getDictumR) where

import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds)
import System.Random (next, mkStdGen)
import Yesod

import Foundation

getDictumR :: Handler Text
getDictumR = do
  time <- liftIO getSystemTime
  let (nonce, _) = next $ mkStdGen $ fromEnum $ systemNanoseconds time
  pure $ case nonce `rem` 5 of
    0 -> "よろづのことは 月見るにこそ慰むものなれ ——《徒然草》"
    1 -> "駒並めていざ見に行かむ 故里は雪とのみこそ花は散るらめ ——《古今集》"
    2 -> "世の中に絶えて桜のなかりせば 春の心はのどけからまし ——《伊勢物語》"
    3 -> "かくばかり恋ひむとかねて知らませば 妹をば見ずぞあるべくありける ——《万葉集》"
    _ -> "桜花散らば散らなむ 散らずとて故里人の来ても見なくに ——《古今集》"
