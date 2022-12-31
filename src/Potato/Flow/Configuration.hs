{-# LANGUAGE RecordWildCards      #-}

module Potato.Flow.Configuration where

import           Relude

import Potato.Data.Text.Unicode 

import Data.Int
import Data.Default


data PotatoConfiguration = PotatoConfiguration {
    _potatoConfiguration_allowGraphemeClusters :: Bool
    , _potatoConfiguration_replaceUnicodeWideCharsWith :: Maybe Char
    , _potatoConfiguration_unicodeWideCharFn :: Char -> Int8
  }

instance Default PotatoConfiguration where
  def = PotatoConfiguration {
      _potatoConfiguration_allowGraphemeClusters = False
      , _potatoConfiguration_replaceUnicodeWideCharsWith = Nothing
      -- NOTE getCharWidth is unsafely modified by vty on initialization based on the terminal config file
      , _potatoConfiguration_unicodeWideCharFn = getCharWidth
    }