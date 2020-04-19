module Potato.Flow.SElts (
  PChar
  , CornerStyle
  , defaultCornerStyle
  , SLineStyle(..)
  , defaultSLineStyle
  , STextStyle(..)
  , SBox(..)
  , SLine(..)
  , SText(..)
  , SElt(..)
  , SEltLabel(..)
  , SEltTree
) where

import           Relude

import           Potato.Flow.Math

import           Data.Aeson

type PChar = Char

data CornerStyle = CornerStyle {
  _cornerStyle_ul   :: PChar
  , _cornerStyle_ur :: PChar
  , _cornerStyle_bl :: PChar
  , _cornerStyle_br :: PChar
} deriving (Eq, Generic, Show)

instance FromJSON CornerStyle
instance ToJSON CornerStyle

defaultCornerStyle :: CornerStyle
defaultCornerStyle = CornerStyle {
    _cornerStyle_ul = '╔'
    , _cornerStyle_ur = '╗'
    , _cornerStyle_bl = '╚'
    , _cornerStyle_br = '╝'
  }

data SLineStyle = SLineStyle {
  sbs_corners      :: CornerStyle
  , sbs_vertical   :: PChar
  , sbs_horizontal :: PChar
} deriving (Eq, Generic, Show)

instance FromJSON SLineStyle
instance ToJSON SLineStyle

defaultSLineStyle :: SLineStyle
defaultSLineStyle = SLineStyle {
    sbs_corners      = defaultCornerStyle
    , sbs_vertical   = '║'
    , sbs_horizontal = '═'
  }

data STextStyle = STextStyle {
  -- margins
} deriving (Eq, Generic, Show)

instance FromJSON STextStyle
instance ToJSON STextStyle

-- | serializable representations of each type of Elt
data SBox = SBox {
  sb_box     :: LBox
  , sb_style :: SLineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SBox
instance ToJSON SBox

-- |
data SLine = SLine {
  sl_start   :: LPoint
  , sl_end   :: LPoint
  , sl_style :: SLineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SLine
instance ToJSON SLine

-- TODO make manipulator
-- TODO rename
-- |
data SCartLines = SCartLines {
  scl_start   :: LPoint
  , scl_ends  :: NonEmpty (Either X Y)
  , scl_style :: SLineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SCartLines
instance ToJSON SCartLines


-- | abitrary text confined to a box
data SText = SText {
  st_box     :: LBox
  , st_text  :: Text
  , st_style :: STextStyle
} deriving (Eq, Generic, Show)

instance FromJSON SText
instance ToJSON SText

data SElt = SEltNone | SEltFolderStart | SEltFolderEnd | SEltBox SBox | SEltLine SLine | SEltText SText deriving (Eq, Generic, Show)

instance FromJSON SElt
instance ToJSON SElt

data SEltLabel = SEltLabel {
 selt_name  :: Text
 , selt_elt :: SElt
} deriving (Generic, Show)

instance FromJSON SEltLabel
instance ToJSON SEltLabel

-- expected to satisfy scoping invariant
type SEltTree = [SEltLabel]
