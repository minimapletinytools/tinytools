
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

internal_getCharacterBreaks :: Text -> [ICU.Break ()]
internal_getCharacterBreaks input = r where 
  breaker = ICU.breakCharacter ICU.Current 
  r = ICU.breaks breaker input

-- | True if the Text is a single grapheme cluster, False otherwise
isSingleGraphemeCluster :: Text -> Bool
isSingleGraphemeCluster input = r where
  tbreaks = internal_getCharacterBreaks input
  r = case tbreaks of 
    -- no characters, not a grapheme cluster
    [] -> False 
    -- only one break, it's a grapheme cluster if it has more than one unicode char in it
    (b:[]) -> T.length (ICU.brkBreak b) > 1
    -- more than one character break
    _ -> False 

-- | True if the last character in the text is a single grapheme cluster, False otherwise
endsInGraphemeCluster :: Text -> Bool
endsInGraphemeCluster input = r where
  tbreaks' = internal_getCharacterBreaks input
  gotoend tbreaks = case tbreaks of 
    [] -> False
    (b:[]) -> isSingleGraphemeCluster (ICU.brkBreak b)
    (_:bs) -> gotoend bs
  r = gotoend tbreaks'

-- most terminals do not support grapheme clusters right now :( 👎🏻👎🏼👎🏽👎🏾👎🏿
-- | removes grapheme clusters from the text and replaces them with the first character in the cluster
removeGraphemeCluster :: Text -> Text
removeGraphemeCluster input = r where
  tbreaks = internal_getCharacterBreaks input
  -- if there is more than one character in the break then it must have been a grapheme cluster
  -- so just use the first character
  fmapfn b = case T.uncons (ICU.brkBreak b) of
    Nothing -> ""
    Just (c, _) -> T.singleton c
  r = mconcat $ fmap fmapfn tbreaks

-- | True if the input text contains a grapheme cluster
containsGraphemeCluster :: Text -> Bool
containsGraphemeCluster input = removeGraphemeCluster input /= input