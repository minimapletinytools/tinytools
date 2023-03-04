
module Potato.Data.Text.Unicode where

import           Prelude

import           Graphics.Text.Width     (wcwidth)

import           Data.Int
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.ICU           as ICU
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

zwidge :: Char
zwidge = '\8205'

-- | True if the Text is a single grapheme cluster, False otherwise
isSingleGraphemeCluster :: Text -> Bool
isSingleGraphemeCluster input = r where
  tbreaks = internal_getCharacterBreaks input
  r = case tbreaks of
    -- no characters, not a grapheme cluster
    []     -> False
    -- only one break, it's a grapheme cluster if it has more than one unicode char in it
    (b:[]) -> T.length (ICU.brkBreak b) > 1
    -- more than one character break
    _      -> False

-- | True if the last character in the text is a single grapheme cluster, False otherwise
endsInGraphemeCluster :: Text -> Bool
endsInGraphemeCluster input = r where
  tbreaks' = internal_getCharacterBreaks input
  gotoend tbreaks = case tbreaks of
    []     -> False
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
    Nothing     -> ""
    Just (c, _) -> T.singleton c
  r = mconcat $ fmap fmapfn tbreaks

-- | True if the input text contains a grapheme cluster
containsGraphemeCluster :: Text -> Bool
containsGraphemeCluster input = removeGraphemeCluster input /= input



-- 🤖 isn't correct, misses emojis :()
{-
getCharWidth :: Char -> Int8
getCharWidth c
  | isControl c || c == '\t' = 0
  | w == 0x0 || w > 0x10ffff = 1
  | w >= 0x1100 && (w <= 0x115f || w == 0x2329 || w == 0x232a || (w >= 0x2e80 && w <= 0xa4cf && w /= 0x303f) || (w >= 0xac00 && w <= 0xd7a3) || (w >= 0xf900 && w <= 0xfaff) || (w >= 0xfe10 && w <= 0xfe19) || (w >= 0xfe30 && w <= 0xfe6f) || (w >= 0xff00 && w <= 0xff60) || (w >= 0xffe0 && w <= 0xffe6) || (w >= 0x20000 && w <= 0x2fffd) || (w >= 0x30000 && w <= 0x3fffd)) = 2
  | otherwise = 1
  where
    w = ord c
-}
