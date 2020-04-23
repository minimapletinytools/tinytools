{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Potato.Flow.Reflex.Types (
  LayerPos
  , REltId
  , REltIdMap
  , PatchREltIdMap
  , SuperSEltLabel
  , ControllersWithId

  -- * manipulators
  , MNone
  , MBox(..)
  , MLine(..)
  , MText(..)
  , MRelBox(..)
  , MTag(..)
  , Manipulator
  -- * controllers
  , CBox(..)
  , CLine(..)
  , CText(..)
  , CRelBox(..)
  , CTag(..)
  , Controller
) where

import           Relude

import           Reflex
import qualified Reflex.Patch.IntMap  as IM

import           Potato.Flow.Math
import           Potato.Flow.SElts

import qualified Data.Dependent.Map   as DM
import qualified Data.Dependent.Sum   as DS
import qualified Data.GADT.Compare
import           Data.GADT.Compare.TH
import qualified Data.IntMap.Strict   as IM
import           Language.Haskell.TH



type LayerPos = Int
type REltId = Int
type REltIdMap a = IM.IntMap a
type PatchREltIdMap a = IM.PatchIntMap a
type SuperSEltLabel = (REltId, LayerPos, SEltLabel)


type MNone = ()

data MBox = MBox {
  _mBox_target :: REltId
  , _mBox_box  :: LBox
}

data MLine = MLine {
  _mLine_target  :: REltId
  , _mLine_start :: LPoint
  , _mLine_end   :: LPoint
}

data MText = MText {
  _mText_target :: REltId
  , _mText_box  :: LBox
  , _mText_text :: Text
}

data MRelBox = MRelBox {
  _mRelBox_targets :: NonEmpty REltId
  , _mRelBox_box   :: LBox
}

data MTag a where
  MTagNone :: MTag MNone
  MTagBox :: MTag MBox
  MTagLine :: MTag MLine
  MTagText :: MTag MText
  MTagRelBox :: MTag MRelBox

deriveGEq      ''MTag
deriveGCompare ''MTag

-- | Manipulators represent starting position of RElts
type Manipulator = DS.DSum MTag Identity



data CBox = CBox {
  _cBox_box :: DeltaLBox
}

data CLine = CLine {
  -- TODO change to DeltaXY
  _cLine_start :: XY
  , _cLine_end :: XY
}

data CText = CText {
  _cText_box    :: DeltaLBox
  , _cText_text :: DeltaText
}

-- | transforms object based on a reference point
-- used for multi-selection
data CRelBox = CRelBox {
  _cRelBox_original :: LBox
  , _cRelBox_box    :: DeltaLBox
}

data CTag a where
  CTagBox :: CTag DeltaLBox
  CTagLine :: CTag CLine
  CTagText :: CTag CText
  CTagRelBox :: CTag CRelBox

deriveGEq      ''CTag
deriveGCompare ''CTag

-- | Controllers represent changes to SELts
type Controller = DS.DSum CTag Identity

type ControllersWithId = IntMap Controller
