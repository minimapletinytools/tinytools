module Potato.Flow.SElts (
  PChar
  , CornerStyle
  , defaultCornerStyle
  , SLineStyle(..)
  , defaultSLineStyle
  , STextStyle(..)
  , defaultSTextStyle
  , SBox(..)
  , SLine(..)
  , SText(..)
  , SElt(..)
  , SEltLabel(..)
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
instance NFData CornerStyle

defaultCornerStyle :: CornerStyle
defaultCornerStyle = CornerStyle {
    _cornerStyle_ul = '╔'
    , _cornerStyle_ur = '╗'
    , _cornerStyle_bl = '╚'
    , _cornerStyle_br = '╝'
  }

-- TODO Rename to just LineStyle
data SLineStyle = SLineStyle {
  _sLineStyle_corners      :: CornerStyle
  , _sLineStyle_vertical   :: PChar
  , _sLineStyle_horizontal :: PChar
} deriving (Eq, Generic, Show)

instance FromJSON SLineStyle
instance ToJSON SLineStyle
instance NFData SLineStyle

defaultSLineStyle :: SLineStyle
defaultSLineStyle = SLineStyle {
    _sLineStyle_corners      = defaultCornerStyle
    , _sLineStyle_vertical   = '║'
    , _sLineStyle_horizontal = '═'
  }

-- TODO rename to TextStyle
data STextStyle = STextStyle {
  -- margins
} deriving (Eq, Generic, Show)

instance FromJSON STextStyle
instance ToJSON STextStyle
instance NFData STextStyle

defaultSTextStyle :: STextStyle
defaultSTextStyle = STextStyle {}

-- |
data SBox = SBox {
  _sBox_box     :: LBox
  , _sBox_style :: SLineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SBox
instance ToJSON SBox
instance NFData SBox

-- |
data SLine = SLine {
  _sLine_start   :: LPoint
  , _sLine_end   :: LPoint
  , _sLine_style :: SLineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SLine
instance ToJSON SLine
instance NFData SLine

-- TODO make manipulator
-- TODO rename
-- |
data SCartLines = SCartLines {
  _sCartLines_start   :: LPoint
  , _sCartLines_ends  :: NonEmpty (Either Int Int)
  , _sCartLines_style :: SLineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SCartLines
instance ToJSON SCartLines
instance NFData SCartLines


-- | abitrary text confined to a box
data SText = SText {
  _sText_box     :: LBox
  , _sText_text  :: Text
  , _sText_style :: STextStyle
} deriving (Eq, Generic, Show)

instance FromJSON SText
instance ToJSON SText
instance NFData SText

data SElt = SEltNone | SEltFolderStart | SEltFolderEnd | SEltBox SBox | SEltLine SLine | SEltText SText deriving (Eq, Generic, Show)

instance FromJSON SElt
instance ToJSON SElt
instance NFData SElt

data SEltLabel = SEltLabel {
 _sEltLabel_name   :: Text
 , _sEltLabel_sElt :: SElt
} deriving (Eq, Generic, Show)

instance FromJSON SEltLabel
instance ToJSON SEltLabel
instance NFData SEltLabel
