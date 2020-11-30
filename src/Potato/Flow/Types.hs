{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Types (
  LayerPos
  , REltId
  , REltIdMap
  , SuperSEltLabel
  , ControllersWithId
  , SEltLabelChanges
  , SEltLabelChangesWithLayerPos

  -- * manipulators
  , MNone
  , MBox(..)
  , MLine(..)
  , MText(..)
  , MBoundingBox(..)
  , MTag(..)
  , Manipulator
  -- * controllers
  , CRename(..)
  , CBox(..)
  , CLine(..)
  , CText(..)
  , CBoundingBox(..)
  , CTag(..)
  , Controller

  , DeltaText
  , DeltaSuperStyle(..)
  , DeltaTextStyle(..)
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts

import           Control.Exception         (assert)
import           Data.Constraint.Extras.TH
import qualified Data.Dependent.Sum        as DS
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import qualified Data.IntMap.Strict        as IM



type LayerPos = Int
type REltId = Int
type REltIdMap a = IM.IntMap a
type SuperSEltLabel = (REltId, LayerPos, SEltLabel)
type SEltLabelChanges = REltIdMap (Maybe SEltLabel)
type SEltLabelChangesWithLayerPos = REltIdMap (Maybe (LayerPos, SEltLabel))

-- TODO DELETE
type MNone = ()

data MBox = MBox {
  _mBox_target :: REltId
  , _mBox_box  :: LBox
} deriving (Eq, Show)

data MLine = MLine {
  _mLine_target  :: REltId
  , _mLine_start :: XY
  , _mLine_end   :: XY
} deriving (Eq, Show)

data MText = MText {
  _mText_target :: REltId
  , _mText_box  :: LBox
  , _mText_text :: Text
} deriving (Eq, Show)

data MBoundingBox = MBoundingBox {
  _mBoundingBox_bounded_targets :: NonEmpty (REltId, LBox)
} deriving (Eq, Show)

-- TODO DELETE MTag stuff
data MTag a where
  MTagNone :: MTag MNone
  MTagBox :: MTag MBox
  MTagLine :: MTag MLine
  MTagText :: MTag MText
  MTagBoundingBox :: MTag MBoundingBox

deriveGEq      ''MTag
deriveGCompare ''MTag
deriveGShow    ''MTag
deriveArgDict  ''MTag





-- | (old text, new text)
type DeltaText = (Text,Text)
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b

-- TODO
data DeltaSuperStyle = DeltaSuperStyle deriving (Eq, Generic, Show)

instance NFData DeltaSuperStyle

-- TODO
instance Delta SuperStyle DeltaSuperStyle where
  plusDelta ss _ = ss
  minusDelta ss _ = ss


-- TODO
data DeltaTextStyle = DeltaTextStyle deriving (Eq, Generic, Show)

instance NFData DeltaTextStyle

-- TODO
instance Delta TextStyle DeltaTextStyle where
  plusDelta ts _ = ts
  minusDelta ts _ = ts




-- TODO you can prob delete this now
-- NOTE, this is no longer used in Reflex event so DSum is not necessary, but there's no reason to change it
type Manipulator = DS.DSum MTag Identity


data CRename = CRename {
  _cRename_deltaLabel :: DeltaText
} deriving (Eq, Generic, Show)

instance NFData CRename

instance Delta SEltLabel CRename where
  plusDelta (SEltLabel name selt) CRename {..} = SEltLabel (plusDelta name _cRename_deltaLabel) selt
  minusDelta (SEltLabel name selt) CRename {..} = SEltLabel (minusDelta name _cRename_deltaLabel) selt

data CBox = CBox {
  _cBox_deltaBox     :: Maybe DeltaLBox
  , _cBox_deltaStyle :: Maybe DeltaSuperStyle
} deriving (Eq, Generic, Show)

instance NFData CBox

instance Delta SBox CBox where
  plusDelta SBox {..} CBox {..} = SBox {
      _sBox_box   = case _cBox_deltaBox of
        Nothing -> _sBox_box
        Just d  -> plusDelta _sBox_box d
      , _sBox_style = case _cBox_deltaStyle of
        Nothing -> _sBox_style
        Just d  -> plusDelta _sBox_style d
    }
  minusDelta SBox {..} CBox {..} = SBox {
      _sBox_box   = case _cBox_deltaBox of
        Nothing -> _sBox_box
        Just d  -> minusDelta _sBox_box d
        , _sBox_style = case _cBox_deltaStyle of
          Nothing -> _sBox_style
          Just d  -> minusDelta _sBox_style d
    }


-- TODO define DeltaXY?
data CLine = CLine {
  _cLine_deltaStart   :: Maybe DeltaXY
  , _cLine_deltaEnd   :: Maybe DeltaXY
  , _cLine_deltaStyle :: Maybe DeltaSuperStyle
} deriving (Eq, Generic, Show)

instance NFData CLine

instance Delta SLine CLine where
  plusDelta SLine {..} CLine {..} = SLine {
      _sLine_start   = case _cLine_deltaStart of
        Nothing -> _sLine_start
        Just d  -> plusDelta _sLine_start d
      , _sLine_end   =  case _cLine_deltaEnd of
        Nothing -> _sLine_end
        Just d  -> plusDelta _sLine_end d
      , _sLine_style = case _cLine_deltaStyle of
        Nothing -> _sLine_style
        Just d  -> plusDelta _sLine_style d
    }
  minusDelta SLine {..} CLine {..} = SLine {
      _sLine_start   = case _cLine_deltaStart of
        Nothing -> _sLine_start
        Just d  -> minusDelta _sLine_start d
        , _sLine_end   =  case _cLine_deltaEnd of
          Nothing -> _sLine_end
          Just d  -> minusDelta _sLine_end d
        , _sLine_style = case _cLine_deltaStyle of
          Nothing -> _sLine_style
          Just d  -> minusDelta _sLine_style d
    }

data CText = CText {
  _cText_deltaBox         :: Maybe DeltaLBox
  , _cText_deltaText      :: Maybe DeltaText
  , _cText_deltaTextStyle :: Maybe DeltaTextStyle
} deriving (Eq, Generic, Show)

instance NFData CText

instance Delta SText CText where
  plusDelta SText {..} CText {..} = SText {
      _sText_box   = case _cText_deltaBox of
        Nothing -> _sText_box
        Just d  -> plusDelta _sText_box d
      , _sText_text   = case _cText_deltaText of
        Nothing -> _sText_text
        Just d  -> plusDelta _sText_text d
      , _sText_style = case _cText_deltaTextStyle of
        Nothing -> _sText_style
        Just d  -> plusDelta _sText_style d
    }
  minusDelta SText {..} CText {..} = SText {
      _sText_box   = case _cText_deltaBox of
        Nothing -> _sText_box
        Just d  -> minusDelta _sText_box d
      , _sText_text   = case _cText_deltaText of
        Nothing -> _sText_text
        Just d  -> minusDelta _sText_text d
      , _sText_style = case _cText_deltaTextStyle of
        Nothing -> _sText_style
        Just d  -> minusDelta _sText_style d
    }

-- | transforms object based on a reference point
-- used for multi-selection
data CBoundingBox = CBoundingBox {
  _cBoundingBox_deltaBox    :: DeltaLBox
} deriving (Eq, Generic, Show)

instance NFData CBoundingBox

data CTag a where
  CTagRename :: CTag CRename
  CTagBox :: CTag CBox
  CTagLine :: CTag CLine
  CTagText :: CTag CText
  CTagBoundingBox :: CTag CBoundingBox

deriveGEq      ''CTag
deriveGCompare ''CTag
deriveGShow ''CTag
deriveArgDict ''CTag

-- | Controllers represent changes to SELts
type Controller = DS.DSum CTag Identity

instance NFData Controller where
  rnf (CTagRename DS.:=> Identity a)      = rnf a
  rnf (CTagBox DS.:=> Identity a)         = rnf a
  rnf (CTagLine DS.:=> Identity a)        = rnf a
  rnf (CTagText DS.:=> Identity a)        = rnf a
  rnf (CTagBoundingBox DS.:=> Identity a) = rnf a



-- | indexed my REltId
type ControllersWithId = IntMap Controller
