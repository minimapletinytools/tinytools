module Potato.Flow.SElts (
  PChar
  , FillStyle(..)
  , CornerStyle(..)
  , SLineStyle(..)
  , STextStyle(..)
  , SBox(..)
  , SLine(..)
  , SText(..)
  , SElt(..)
  , SEltLabel(..)
  , SEltTree
  , SCanvas(..)
  , SPotatoFlow(..)
) where

import           Relude

import           Potato.Flow.Math

import           Data.Aeson
import           Data.Default

type PChar = Char

data FillStyle = FillSimple PChar deriving (Eq, Generic, Show)

instance FromJSON FillStyle
instance ToJSON FillStyle
instance NFData FillStyle

instance Default FillStyle where
  def = FillSimple '@'

data CornerStyle = CornerStyle {
  _cornerStyle_ul   :: PChar
  , _cornerStyle_ur :: PChar
  , _cornerStyle_bl :: PChar
  , _cornerStyle_br :: PChar
} deriving (Eq, Generic, Show)

instance FromJSON CornerStyle
instance ToJSON CornerStyle
instance NFData CornerStyle

instance Default CornerStyle where
  def = CornerStyle {
      _cornerStyle_ul = '╔'
      , _cornerStyle_ur = '╗'
      , _cornerStyle_bl = '╚'
      , _cornerStyle_br = '╝'
    }

-- TODO Rename to just LineStyle
-- TODO default instances
data SLineStyle = SLineStyle {
  _sLineStyle_corners      :: CornerStyle
  , _sLineStyle_vertical   :: PChar
  , _sLineStyle_horizontal :: PChar
  , _sLineStyle_point      :: PChar
  , _sLineStyle_fill       :: FillStyle
} deriving (Eq, Generic, Show)

instance FromJSON SLineStyle
instance ToJSON SLineStyle
instance NFData SLineStyle

instance Default SLineStyle where
  def = SLineStyle {
    _sLineStyle_corners      = def
    , _sLineStyle_vertical   = '║'
    , _sLineStyle_horizontal = '═'
    , _sLineStyle_point = '█'
    , _sLineStyle_fill = def
  }

-- TODO rename to TextStyle
data STextStyle = STextStyle {
  -- margins
} deriving (Eq, Generic, Show)

instance FromJSON STextStyle
instance ToJSON STextStyle
instance NFData STextStyle

instance Default STextStyle where
  def = STextStyle {}

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

type SEltTree = [SEltLabel]

data SCanvas = SCanvas {
  _sCanvas_box :: LBox
} deriving (Eq, Generic, Show)

instance FromJSON SCanvas
instance ToJSON SCanvas
instance NFData SCanvas

data SPotatoFlow = SPotatoFlow {
  _sPotatoFlow_sCanvas    :: SCanvas
  , _sPotatoFlow_sEltTree :: SEltTree
} deriving (Eq, Generic, Show)

instance FromJSON SPotatoFlow
instance ToJSON SPotatoFlow
instance NFData SPotatoFlow
