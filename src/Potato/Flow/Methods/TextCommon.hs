{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.TextCommon (
  displayLinesToChar
) where

import           Relude

import           Potato.Flow.Serialization.Snake

import qualified Data.Map as Map
import qualified Data.Text                      as T
import qualified Potato.Data.Text.Zipper        as TZ




concatSpans :: [TZ.Span a] -> Text
concatSpans spans = mconcat $ fmap (\(TZ.Span _ t) -> t) spans

subWidth :: Text -> [Maybe Char]
subWidth t = join . fmap fn . T.unpack $ t where
  fn c = case TZ.charWidth c of
    1 -> [Just c]
    2 -> [Just c, Nothing]
    n -> [Nothing]
    --n -> trace ("unexpected char " <> [c] <> " of width " <> show n) [Nothing]



displayLinesToChar ::
  (Int, Int) -- ^ the upper left corner of the box containing the text we want to render
  -> TZ.DisplayLines Int -- ^ pre-generated displaylines
  -> (Int, Int) -- ^ the point we want to render
  -> (Int, Int) -- ^ how much text is offest by
  -> Maybe MPChar
displayLinesToChar (x, y) dl (x',y') (xoff, yoff) = outputChar where
  spans = TZ._displayLines_spans dl
  offsetMap = TZ._displayLines_offsetMap dl
  yidx = y' - y - yoff
  xalignoffset = case Map.lookup yidx offsetMap of
    -- this will happen because the last character in spans is a generated cursor character if the cursor is at the end and the text ends with a new line
    Nothing -> -1
    --Nothing -> error $ "should not happen. got " <> show yidx <> " in\n" <> show dl <> "\n" <> show spans <> "\n" <> show offsetMap
    Just (offset,_) -> offset
  outputChar = case spans !!? yidx of
    Nothing -> Nothing
    Just row -> outputChar' where
      rowText = subWidth $ concatSpans row
      xidx = x' - x - xoff - xalignoffset
      outputChar' = case rowText !!? xidx of
        Nothing   -> Nothing
        Just cell -> Just cell
