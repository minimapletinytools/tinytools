{-# LANGUAGE RecordWildCards #-}

module Potato.GlyphFether where

import           Network.HTTP.Simple
import qualified Data.Text as T
import           Control.Exception (try)


data GlyphIndex = GlpyhIndex 
{
  _glyphIndex_name :: Text
  , _glyphIndex_description :: Text
  , _glyphIndex_tags :: [Text]
  , _glyphIndex_filename :: Text
  , _glyphIndex_size :: (Int, Int)
}

data Glyph = Glyph
{
  _glyph_contents :: Text
  , _glyph_glyphIndex :: GlyphIndex
}

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe io = try io >>= \case
  Right x -> return (Just x)
  Left _ -> return Nothing

fetchIndex :: IO Text
fetchIndex = do
  resp <- httpLBS "https://raw.githubusercontent.com/pdlla/tiny-tools/testindex.txt"
  return $ LT.toStrict $ LT.decodeUtf8 (getResponseBody resp)

parseIndex :: Text -> []
parseIndex input = undefined

fetchGlyph :: GlyphIndex -> IO (Maybe Glyph)
fetchGlyph gi = do
  mresp <- tryMaybe $ httpLBS "https://raw.githubusercontent.com/pdlla/tiny-tools/testindex.txt"
  return $ case mresp of
    Nothing -> Nothing
    Just gt -> Glyph {
        _glyph_ontents = gt
        , _glyph_glyphIndex = gi  
      }

