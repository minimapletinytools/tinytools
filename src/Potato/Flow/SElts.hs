module Potato.Flow.SElts (
  SLineStyle(..)
  , STextStyle(..)
  , SBox(..)
  , SLine(..)
  , SText(..)
  , SElt(..)
  , SEltTree
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Types

import           Data.Aeson


data SLineStyle = SLineStyle {
  sbs_corners      :: PChar
  , sbs_vertical   :: PChar
  , sbs_horizontal :: PChar
} deriving (Generic, Show)

instance FromJSON SLineStyle
instance ToJSON SLineStyle

data STextStyle = STextStyle {
  -- margins
} deriving (Generic, Show)

instance FromJSON STextStyle
instance ToJSON STextStyle

-- | serializable representations of each type of Elt
data SBox = SBox {
  sb_box     :: LBox
  , sb_style :: SLineStyle
} deriving (Generic, Show)

instance FromJSON SBox
instance ToJSON SBox

-- |
data SLine = SLine {
  sl_start   :: LPoint
  , sl_ends  :: NonEmpty (Either X Y)
  , sl_style :: SLineStyle
} deriving (Generic, Show)

instance FromJSON SLine
instance ToJSON SLine

-- | abitrary text confined to a box
data SText = SText {
  st_box     :: LBox
  , st_style :: STextStyle
} deriving (Generic, Show)

instance FromJSON SText
instance ToJSON SText

-- TODO switch this to DSum
data SElt = SEltNone | SEltFolderStart | SEltFolderEnd | SEltBox SBox | SEltLine SLine | SEltText SText deriving (Generic, Show)

instance FromJSON SElt
instance ToJSON SElt

type SEltTree = [SElt]
