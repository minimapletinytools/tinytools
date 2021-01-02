{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SElts (
  PChar
  , FillStyle(..)
  , SuperStyle(..)
  , superStyle_fromListFormat
  , superStyle_toListFormat
  , TextStyle(..)
  , SBoxTitle(..)
  , SBoxText(..)
  , SBoxType(..)
  , sBoxType_isText
  , SBox(..)
  , SLine(..)
  , STextArea(..)
  , SElt(..)
  , SEltLabel(..)
) where

import           Relude

import           Potato.Flow.Math

import           Control.Exception (assert)
import           Data.Aeson
import           Data.Binary
import           Data.Default
import qualified Data.List         as L

type PChar = Char

data FillStyle = FillStyle_Blank | FillStyle_Simple PChar deriving (Eq, Generic, Show)

instance FromJSON FillStyle
instance ToJSON FillStyle
instance Binary FillStyle
instance NFData FillStyle

instance Default FillStyle where
  def = FillStyle_Simple '@'


-- TODO add line ends?
-- TODO add line thickness?
-- TODO add line fill?
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
instance Binary SuperStyle
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

superStyle_fromListFormat :: [PChar] -> SuperStyle
superStyle_fromListFormat chars = assert (l == 7 || l == 8) $ r where
  l = length chars
  r = SuperStyle {
    _superStyle_tl = chars L.!! 0
    , _superStyle_tr = chars L.!! 1
    , _superStyle_bl = chars L.!! 2
    , _superStyle_br = chars L.!! 3
    , _superStyle_vertical   = chars L.!! 4
    , _superStyle_horizontal = chars L.!! 5
    , _superStyle_point = chars L.!! 6
    , _superStyle_fill = if l == 7 then def else FillStyle_Simple (chars L.!! 7)
  }

-- TODO test
-- superStyle_fromListFormat "╔╗╚╝║═█" `shouldBe` def

superStyle_toListFormat :: SuperStyle -> [PChar]
superStyle_toListFormat SuperStyle {..} = r where
  start = case _superStyle_fill of
    FillStyle_Blank    -> []
    FillStyle_Simple c -> [c]
  r = start <> [
      _superStyle_tl
      , _superStyle_tr
      , _superStyle_bl
      , _superStyle_br
      , _superStyle_vertical
      , _superStyle_horizontal
      , _superStyle_point
    ]

-- |
data TextAlign = TextAlign_Left | TextAlign_Right | TextAlign_Center | TextAlign_Justify deriving (Eq, Generic, Show)

instance FromJSON TextAlign
instance ToJSON TextAlign
instance Binary TextAlign
instance NFData TextAlign

instance Default TextAlign where
  def = TextAlign_Left

-- |
data TextStyle = TextStyle {
  -- margins
  _textStyle_alignment :: TextAlign
} deriving (Eq, Generic, Show)

instance FromJSON TextStyle
instance ToJSON TextStyle
instance Binary TextStyle
instance NFData TextStyle

instance Default TextStyle where
  def = TextStyle { _textStyle_alignment = def }

-- |
data SBoxTitle = SBoxTitle {
  _sBoxTitle_title   :: Maybe Text
  , _sBoxTitle_align :: TextAlign
} deriving (Eq, Generic, Show)

instance FromJSON SBoxTitle
instance ToJSON SBoxTitle
instance Binary SBoxTitle
instance NFData SBoxTitle

instance Default SBoxTitle where
  def = SBoxTitle {
      _sBoxTitle_title = Nothing
      , _sBoxTitle_align = def
    }

-- |
data SBoxText = SBoxText {
  _sBoxText_text    :: Text
  , _sBoxText_style :: TextStyle
} deriving (Eq, Generic, Show)

instance FromJSON SBoxText
instance ToJSON SBoxText
instance Binary SBoxText
instance NFData SBoxText

instance Default SBoxText where
  def = SBoxText {
      _sBoxText_text = ""
      , _sBoxText_style = def
    }

data SBoxType = SBoxType_Box | SBoxType_BoxText | SBoxType_NoBoxText deriving (Eq, Generic, Show)

instance FromJSON SBoxType
instance ToJSON SBoxType
instance Binary SBoxType
instance NFData SBoxType

sBoxType_isText :: SBoxType -> Bool
sBoxType_isText = (/=) SBoxType_Box

-- |
data SBox = SBox {
  _sBox_box       :: LBox
  , _sBox_style   :: SuperStyle
  , _sBox_title   :: SBoxTitle
  , _sBox_text    :: SBoxText
  , _sBox_boxType :: SBoxType
} deriving (Eq, Generic, Show)

instance FromJSON SBox
instance ToJSON SBox
instance Binary SBox
instance NFData SBox

instance Default SBox where
  def = SBox {
      _sBox_box     = LBox 0 0
      , _sBox_style = def
      , _sBox_title = def
      , _sBox_text  = def
      , _sBox_boxType = SBoxType_Box
    }

-- |
data SLine = SLine {
  _sLine_start   :: XY
  , _sLine_end   :: XY
  , _sLine_style :: SuperStyle
  -- TODO arrows heads (maybe just make it part of SuperStyle?)
} deriving (Eq, Generic, Show)

instance FromJSON SLine
instance ToJSON SLine
instance Binary SLine
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
instance Binary SCartLines
instance NFData SCartLines


-- | abitrary text confined to a box
data STextArea = STextArea {
  _sTextArea_box           :: LBox
  -- TODO probably Map (Int,Int) Char
  , _sTextArea_text        :: Text
  -- TODO consider using SuperStyle here instead and using Fill property only
  , _sTextArea_transparent :: Bool
} deriving (Eq, Generic, Show)

instance FromJSON STextArea
instance ToJSON STextArea
instance Binary STextArea
instance NFData STextArea

-- TODO consider changing this to DSum? Nah probably not, just asking for trouble for making pattern matching not actually any simpler
data SElt = SEltNone | SEltFolderStart | SEltFolderEnd | SEltBox SBox | SEltLine SLine | SEltTextArea STextArea deriving (Eq, Generic, Show)

instance FromJSON SElt
instance ToJSON SElt
instance Binary SElt
instance NFData SElt

data SEltLabel = SEltLabel {
 _sEltLabel_name   :: Text
 , _sEltLabel_sElt :: SElt
} deriving (Eq, Generic, Show)

instance FromJSON SEltLabel
instance ToJSON SEltLabel
instance Binary SEltLabel
instance NFData SEltLabel
