module Potato.Flow.SElts (
  PChar
  , FillStyle(..)
  , SuperStyle(..)
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


-- TODO maybe rename to SuperStyle?
data SuperStyle = SuperStyle {
  _superStyle_tl           :: PChar
  , _superStyle_tr         :: PChar
  , _superStyle_bl         :: PChar
  , _superStyle_br         :: PChar
  , _superStyle_vertical   :: PChar
  , _superStyle_horizontal :: PChar
  , _superStyle_point      :: PChar
  , _superStyle_fill       :: FillStyle
} deriving (Eq, Generic, Show)

instance FromJSON SuperStyle
instance ToJSON SuperStyle
instance NFData SuperStyle

instance Default SuperStyle where
  def = SuperStyle {
    _superStyle_tl = '╔'
    , _superStyle_tr = '╗'
    , _superStyle_bl = '╚'
    , _superStyle_br = '╝'
    , _superStyle_vertical   = '║'
    , _superStyle_horizontal = '═'
    , _superStyle_point = '█'
    , _superStyle_fill = def
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
  , _sBox_style :: SuperStyle
} deriving (Eq, Generic, Show)

instance FromJSON SBox
instance ToJSON SBox
instance NFData SBox

-- |
data SLine = SLine {
  _sLine_start   :: XY
  , _sLine_end   :: XY
  , _sLine_style :: SuperStyle
  -- TODO arrows heads (maybe just make it part of SuperStyle?)
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
  , _sCartLines_style :: SuperStyle
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
