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
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts

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

-- TODO you can prob delete this now
-- NOTE, this is no longer used in Reflex event so DSum is not necessary, but there's no reason to change it
type Manipulator = DS.DSum MTag Identity


data CRename = CRename {
  _cRename_deltaLabel :: DeltaText
} deriving (Eq, Show)

instance Delta SEltLabel CRename where
  plusDelta (SEltLabel name selt) CRename {..} = SEltLabel (plusDelta name _cRename_deltaLabel) selt
  minusDelta (SEltLabel name selt) CRename {..} = SEltLabel (minusDelta name _cRename_deltaLabel) selt

data CBox = CBox {
  _cBox_deltaBox :: DeltaLBox
} deriving (Eq, Show)

instance Delta SBox CBox where
  plusDelta SBox {..} CBox {..} = SBox {
      _sBox_box   = plusDelta _sBox_box _cBox_deltaBox
      , _sBox_style = _sBox_style
    }
  minusDelta SBox {..} CBox {..} = SBox {
      _sBox_box   = minusDelta _sBox_box _cBox_deltaBox
      , _sBox_style = _sBox_style
    }


data CLine = CLine {
  _cLine_deltaStart :: XY
  , _cLine_deltaEnd :: XY
} deriving (Eq, Show)

instance Delta SLine CLine where
  plusDelta SLine {..} CLine {..} = SLine {
      _sLine_start   = plusDelta _sLine_start _cLine_deltaStart
      , _sLine_end   = plusDelta _sLine_end _cLine_deltaEnd
      , _sLine_style = _sLine_style
    }
  minusDelta SLine {..} CLine {..} = SLine {
      _sLine_start   = minusDelta _sLine_start _cLine_deltaStart
      , _sLine_end   = minusDelta _sLine_end _cLine_deltaEnd
      , _sLine_style = _sLine_style
    }

data CText = CText {
  _cText_deltaBox    :: DeltaLBox
  , _cText_deltaText :: DeltaText
} deriving (Eq, Show)

instance Delta SText CText where
  plusDelta SText {..} CText {..} = SText {
      _sText_box   = plusDelta _sText_box _cText_deltaBox
      , _sText_text   = plusDelta _sText_text _cText_deltaText
      , _sText_style = _sText_style
    }
  minusDelta SText {..} CText {..} = SText {
      _sText_box   = minusDelta _sText_box _cText_deltaBox
      , _sText_text   = minusDelta _sText_text _cText_deltaText
      , _sText_style = _sText_style
    }

-- | transforms object based on a reference point
-- used for multi-selection
data CBoundingBox = CBoundingBox {
  _cBoundingBox_deltaBox    :: DeltaLBox
} deriving (Eq, Show)

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

-- | indexed my REltId
type ControllersWithId = IntMap Controller
