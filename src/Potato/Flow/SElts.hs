{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SElts (
  REltId
  , PChar
  , MPChar
  , FillStyle(..)
  , SuperStyle(..)
  , superStyle_fromListFormat
  , superStyle_toListFormat
  , TextAlign(..)
  , convertTextAlignToTextZipperTextAlignment
  , TextStyle(..)
  , LineStyle(..)
  , LineAutoStyle(..)
  , AttachmentLocation(..)
  , Attachment(..)
  , SBoxTitle(..)
  , SBoxText(..)
  , SBoxType(..)
  , sBoxType_isText
  , sBoxType_hasBorder
  , make_sBoxType
  , SBox(..)
  , sBox_hasLabel
  , SSimpleLine(..)
  , TextAreaMapping
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
import qualified Data.Map as Map
import qualified Potato.Data.Text.Zipper                          as TZ

type REltId = Int
type PChar = Char
type MPChar = Maybe PChar

data FillStyle = FillStyle_Blank | FillStyle_Simple PChar deriving (Eq, Generic, Show)

instance FromJSON FillStyle
instance ToJSON FillStyle
instance Binary FillStyle
instance NFData FillStyle

instance Default FillStyle where
  -- TODO change to ' ' prob
  def = FillStyle_Simple '@'

-- TODO add line ends?
-- TODO add line thickness?
-- TODO add line fill?
data SuperStyle = SuperStyle {
  _superStyle_tl           :: MPChar
  , _superStyle_tr         :: MPChar
  , _superStyle_bl         :: MPChar
  , _superStyle_br         :: MPChar
  , _superStyle_vertical   :: MPChar
  , _superStyle_horizontal :: MPChar
  , _superStyle_point      :: MPChar -- used for 1x1 boxes and 1x lines
  , _superStyle_fill       :: FillStyle
} deriving (Eq, Generic, Show)

instance FromJSON SuperStyle
instance ToJSON SuperStyle
instance Binary SuperStyle
instance NFData SuperStyle

instance Default SuperStyle where
  def = SuperStyle {
    _superStyle_tl = Just '╔'
    , _superStyle_tr = Just '╗'
    , _superStyle_bl = Just '╚'
    , _superStyle_br = Just '╝'
    , _superStyle_vertical   = Just '║'
    , _superStyle_horizontal = Just '═'
    , _superStyle_point = Just '█'
    , _superStyle_fill = def
  }

superStyle_fromListFormat :: [PChar] -> SuperStyle
superStyle_fromListFormat chars = assert (l == 7 || l == 8) $ r where
  l = length chars
  r = SuperStyle {
    _superStyle_tl = Just $ chars L.!! 0
    , _superStyle_tr = Just $ chars L.!! 1
    , _superStyle_bl = Just $ chars L.!! 2
    , _superStyle_br = Just $ chars L.!! 3
    , _superStyle_vertical   = Just $ chars L.!! 4
    , _superStyle_horizontal = Just $ chars L.!! 5
    , _superStyle_point = Just $ chars L.!! 6
    , _superStyle_fill = if l == 7 then FillStyle_Blank else FillStyle_Simple (chars L.!! 7)
  }

-- TODO test
-- superStyle_fromListFormat "╔╗╚╝║═█" `shouldBe` def
-- empty styles are converted to space character
superStyle_toListFormat :: SuperStyle -> [PChar]
superStyle_toListFormat SuperStyle {..} = r where
  mfill = case _superStyle_fill of
    FillStyle_Blank    -> []
    FillStyle_Simple c -> [c]
  r = [
      fromMaybe ' ' _superStyle_tl
      ,fromMaybe ' ' _superStyle_tr
      ,fromMaybe ' ' _superStyle_bl
      ,fromMaybe ' ' _superStyle_br
      ,fromMaybe ' ' _superStyle_vertical
      ,fromMaybe ' ' _superStyle_horizontal
      ,fromMaybe ' ' _superStyle_point
    ] <> mfill

-- |
data TextAlign = TextAlign_Left | TextAlign_Right | TextAlign_Center deriving (Eq, Generic, Show)

instance FromJSON TextAlign
instance ToJSON TextAlign
instance Binary TextAlign
instance NFData TextAlign

instance Default TextAlign where
  def = TextAlign_Left

convertTextAlignToTextZipperTextAlignment :: TextAlign -> TZ.TextAlignment
convertTextAlignToTextZipperTextAlignment = \case
  TextAlign_Left -> TZ.TextAlignment_Left
  TextAlign_Right -> TZ.TextAlignment_Right
  TextAlign_Center -> TZ.TextAlignment_Center

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


-- TODO you need support for AL_Any and maybe AL_Center
-- TODO lowercase plz
data AttachmentLocation = AL_Top | AL_Bot | AL_Left | AL_Right | AL_Any deriving (Eq, Generic, Show)

instance FromJSON AttachmentLocation
instance ToJSON AttachmentLocation
instance Binary AttachmentLocation
instance NFData AttachmentLocation

data Attachment = Attachment {
  _attachment_target :: REltId
  , _attachment_location :: AttachmentLocation
  -- you can prob just delete these, don't think we need them.
  -- + is right or down - is left or up
  -- , _attach_offset_abs :: Int
  -- , _attach_offset_rel :: Float
} deriving (Eq, Generic, Show)

instance FromJSON Attachment
instance ToJSON Attachment
instance Binary Attachment
instance NFData Attachment

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

data SBoxType = SBoxType_Box | SBoxType_NoBox | SBoxType_BoxText | SBoxType_NoBoxText deriving (Eq, Generic, Show)

instance FromJSON SBoxType
instance ToJSON SBoxType
instance Binary SBoxType
instance NFData SBoxType

instance Default SBoxType where
  def = SBoxType_Box

sBoxType_isText :: SBoxType -> Bool
sBoxType_isText sbt = sbt == SBoxType_BoxText || sbt == SBoxType_NoBoxText

sBoxType_hasBorder :: SBoxType -> Bool
sBoxType_hasBorder sbt = sbt == SBoxType_Box || sbt == SBoxType_BoxText

make_sBoxType :: Bool -> Bool -> SBoxType
make_sBoxType border text = if border
  then if text
    then SBoxType_BoxText
    else SBoxType_Box
  else if text
    then SBoxType_NoBoxText
    else SBoxType_NoBox

-- |
data SBox = SBox {
  _sBox_box       :: LBox
  -- TODO Rename to _superStyle
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

sBox_hasLabel :: SBox -> Bool
sBox_hasLabel sbox = sBoxType_hasBorder (_sBox_boxType sbox) && (isJust . _sBoxTitle_title ._sBox_title $ sbox)

data LineAutoStyle =
  LineAutoStyle_Auto
  | LineAutoStyle_AutoStraight
  | LineAutoStyle_StraightAlwaysHorizontal
  | LineAutoStyle_StraightAlwaysVertical
  deriving (Eq, Generic, Show)

instance FromJSON LineAutoStyle
instance ToJSON LineAutoStyle
instance Binary LineAutoStyle
instance NFData LineAutoStyle

instance Default LineAutoStyle where
  def = LineAutoStyle_AutoStraight

data LineStyle = LineStyle {
  _lineStyle_leftArrows    :: Text
  , _lineStyle_rightArrows :: Text
  , _lineStyle_upArrows    :: Text
  , _lineStyle_downArrows  :: Text
  , _lineStyle_autoStyle   :: LineAutoStyle
} deriving (Eq, Generic, Show)

instance FromJSON LineStyle
instance ToJSON LineStyle
instance Binary LineStyle
instance NFData LineStyle

instance Default LineStyle where
  def = LineStyle {
      _lineStyle_leftArrows    = "<"
      , _lineStyle_rightArrows = ">"
      , _lineStyle_upArrows    = "^"
      , _lineStyle_downArrows  = "v"
      , _lineStyle_autoStyle   = def
    }

-- |
data SSimpleLine = SSimpleLine {
  _sSimpleLine_start       :: XY
  , _sSimpleLine_end       :: XY
  -- TODO Rename to _superStyle
  , _sSimpleLine_style     :: SuperStyle
  , _sSimpleLine_lineStyle :: LineStyle

  , _sSimpleLine_attachStart :: Maybe Attachment
  , _sSimpleLine_attachEnd :: Maybe Attachment

  , _sSimpleLine_midpoints :: [XY]
} deriving (Eq, Generic, Show)

instance FromJSON SSimpleLine
instance ToJSON SSimpleLine
instance Binary SSimpleLine
instance NFData SSimpleLine

-- makes writing tests easier...
instance Default SSimpleLine where
  def = SSimpleLine {
      _sSimpleLine_start       = 0
      , _sSimpleLine_end       = 0
      , _sSimpleLine_style     = def
      , _sSimpleLine_lineStyle = def
      , _sSimpleLine_attachStart = Nothing
      , _sSimpleLine_attachEnd = Nothing
      , _sSimpleLine_midpoints = []
    }

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

type TextAreaMapping = Map XY PChar

-- | abitrary text confined to a box
data STextArea = STextArea {
  _sTextArea_box           :: LBox
  , _sTextArea_text        :: TextAreaMapping
  -- TODO consider using SuperStyle here instead and using Fill property only
  , _sTextArea_transparent :: Bool
} deriving (Eq, Generic, Show)

instance Default STextArea where
  def = STextArea {
      _sTextArea_box   =        LBox 0 0
      , _sTextArea_text        = Map.empty
      , _sTextArea_transparent = True
    }

instance FromJSON STextArea
instance ToJSON STextArea
instance Binary STextArea
instance NFData STextArea

-- TODO consider removing Folder
data SElt =
  SEltNone
  | SEltFolderStart
  | SEltFolderEnd
  | SEltBox SBox
  | SEltLine SSimpleLine -- TODO rename to SEltSimpleLine?
  | SEltTextArea STextArea
  deriving (Eq, Generic, Show)

instance FromJSON SElt
instance ToJSON SElt
instance Binary SElt
instance NFData SElt

-- TODO consider removing this all together and serializing Owl stuff directly
data SEltLabel = SEltLabel {
 _sEltLabel_name   :: Text
 , _sEltLabel_sElt :: SElt
} deriving (Eq, Generic, Show)

instance FromJSON SEltLabel
instance ToJSON SEltLabel
instance Binary SEltLabel
instance NFData SEltLabel
