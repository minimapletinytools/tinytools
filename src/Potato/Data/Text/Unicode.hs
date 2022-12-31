
module Potato.Data.Text.Unicode where

import           Prelude

import Graphics.Text.Width (wcwidth)

import Data.Int 
import qualified Data.Text.ICU as ICU
import qualified Data.Text as T
import Data.Text (Text)
import qualified Potato.Data.Text.Zipper as TZ



-- NOTE this function won't work as expected until you've loaded a termal char width file via vty!
getCharWidth :: Char -> Int8
getCharWidth = fromIntegral . TZ.charWidth

removeWideChars :: Text -> Text
removeWideChars = T.filter (\c -> getCharWidth c <= 1) 

-- most terminals do not support grapheme clusters right now :( 👎🏻👎🏼👎🏽👎🏾👎🏿
removeGraphemeCluster :: Text -> Text
removeGraphemeCluster input = r where
  breaker = ICU.breakCharacter ICU.Current 
  tbreaks = ICU.breaks breaker input
  -- if there is more than one character in the break then it must have been a grapheme cluster
  -- so just use the first character
  fmapfn b = case T.uncons (ICU.brkBreak b) of
    Nothing -> ""
    Just (c, _) -> T.singleton c
  r = mconcat $ fmap fmapfn tbreaks
