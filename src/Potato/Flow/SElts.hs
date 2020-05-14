module Potato.Flow.SElts (
  PChar
  , FillStyle(..)
  , CornerStyle(..)
  , LineStyle(..)
  , TextStyle(..)
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

data FillStyle = FillStyle_Blank | FillStyle_Simple PChar deriving (Eq, Generic, Show)

instance FromJSON FillStyle
instance ToJSON FillStyle
instance NFData FillStyle

instance Default FillStyle where
  def = FillStyle_Simple '@'

data CornerStyle = CornerStyle {
  _cornerStyle_tl   :: PChar
  , _cornerStyle_tr :: PChar
  , _cornerStyle_bl :: PChar
  , _cornerStyle_br :: PChar
} deriving (Eq, Generic, Show)

instance FromJSON CornerStyle
instance ToJSON CornerStyle
instance NFData CornerStyle

instance Default CornerStyle where
  def = CornerStyle {
      _cornerStyle_tl = '╔'
      , _cornerStyle_tr = '╗'
      , _cornerStyle_bl = '╚'
      , _cornerStyle_br = '╝'
    }

data LineStyle = LineStyle {
  _lineStyle_corners      :: CornerStyle
  , _lineStyle_vertical   :: PChar
  , _lineStyle_horizontal :: PChar
  , _lineStyle_point      :: PChar
  , _lineStyle_fill       :: FillStyle
} deriving (Eq, Generic, Show)

instance FromJSON LineStyle
instance ToJSON LineStyle
instance NFData LineStyle

instance Default LineStyle where
  def = LineStyle {
    _lineStyle_corners      = def
    , _lineStyle_vertical   = '║'
    , _lineStyle_horizontal = '═'
    , _lineStyle_point = '█'
    , _lineStyle_fill = def
  }

data TextAlign = TextAlign_Left | TextAlign_Right | TextAlign_Center | TextAlign_Justify deriving (Eq, Generic, Show)

instance FromJSON TextAlign
instance ToJSON TextAlign
instance NFData TextAlign

instance Default TextAlign where
  def = TextAlign_Left

data TextStyle = TextStyle {
  -- margins
  -- alignment
  _textStyle_alignment :: TextAlign
} deriving (Eq, Generic, Show)

instance FromJSON TextStyle
instance ToJSON TextStyle
instance NFData TextStyle

instance Default TextStyle where
  def = TextStyle { _textStyle_alignment = def }

-- |
data SBox = SBox {
  _sBox_box     :: LBox
  , _sBox_style :: LineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SBox
instance ToJSON SBox
instance NFData SBox

-- |
data SLine = SLine {
  _sLine_start   :: XY
  , _sLine_end   :: XY
  , _sLine_style :: LineStyle
  -- TODO arrows heads (maybe just make it part of LineStyle?)
} deriving (Eq, Generic, Show)

instance FromJSON SLine
instance ToJSON SLine
instance NFData SLine

-- TODO make manipulator
-- TODO rename
-- |
data SCartLines = SCartLines {
  _sCartLines_start   :: XY
  , _sCartLines_ends  :: NonEmpty (Either Int Int)
  , _sCartLines_style :: LineStyle
} deriving (Eq, Generic, Show)

instance FromJSON SCartLines
instance ToJSON SCartLines
instance NFData SCartLines


-- | abitrary text confined to a box
data SText = SText {
  _sText_box     :: LBox
  , _sText_text  :: Text
  , _sText_style :: TextStyle
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
