{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Configuration where

import           Relude

import           Potato.Data.Text.Unicode

import           Data.Default
import qualified Text.Show

data PotatoConfiguration = PotatoConfiguration {
    _potatoConfiguration_allowGraphemeClusters            :: Bool -- NOTE we don't support replacing grapheme clusters as this would require reverting past inputs
    -- TODO rename to disallow
    , _potatoConfiguration_allowOrReplaceUnicodeWideChars :: Maybe (Maybe Char) -- outer Maybe is if we disallow, inner Maybe is what we replace with
    , _potatoConfiguration_unicodeWideCharFn              :: Char -> Int8
  }

instance Show PotatoConfiguration where
  show PotatoConfiguration {..} = "_potatoConfiguration_allowGraphemeClusters: " <> show _potatoConfiguration_allowGraphemeClusters <> "\n_potatoConfiguration_allowOrReplaceUnicodeWideChars: " <> show _potatoConfiguration_allowOrReplaceUnicodeWideChars

instance Default PotatoConfiguration where
  def = PotatoConfiguration {
      _potatoConfiguration_allowGraphemeClusters = False
      --, _potatoConfiguration_allowOrReplaceUnicodeWideChars = Just (Just '☺')
      , _potatoConfiguration_allowOrReplaceUnicodeWideChars = Nothing
      -- NOTE getCharWidth is unsafely modified by vty on initialization based on the terminal config file
      , _potatoConfiguration_unicodeWideCharFn = getCharWidth
    }
