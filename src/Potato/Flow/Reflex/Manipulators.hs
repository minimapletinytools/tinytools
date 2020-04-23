{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}

-- TODO consider moving to Types.Manipulators
module Potato.Flow.Reflex.Manipulators
  ( MNone
  , MBox(..)
  , MLine(..)
  , MText(..)
  , MRelBox(..)
  , MTag(..)
  , Manipulator
  , CBox(..)
  , CLine(..)
  , CText(..)
  , CRelBox(..)
  , CTag(..)
  , Controller
  )
where

import           Relude

import           Reflex

import           Potato.Flow.Math

import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS
--import qualified Data.GADT.Compare


-- pattern for piping manipulator back into PFC
-- start with: _pfo_allElts     :: Behavior t (Map LayerEltId (REltLabel t))
-- convert to
-- selected :: Dynamic t [REltLabel t]
-- convert to
-- manipulators = join . fmap (..) selecetd :: Dynamic t [Manipulators t]
-- convert to
-- manipulator :: Dynamic t ManipulatorWidget
-- render (reflex-dom)
-- manipEv = dyn manipulator :: m (Event t (Controllers t))
-- pass pack to PFC
-- rec _pfc_manipulate = manipEv

data MTag t a where
  MTagNone :: MTag t MNone
  MTagBox :: MTag t (MBox t)
  MTagLine :: MTag t (MLine t)
  MTagText :: MTag t (MText t)
  MTagRelBox :: MTag t (MRelBox t)
  -- TODO manual instances needed
  --deriving anyclass Data.GADT.Compare.GEq
  --deriving anyclass DM.GCompare

type MNone = ()

data MBox t = MBox {
  _mBox_box :: Dynamic t LBox
}

data MLine t = MLine {
  _mLine_start :: Dynamic t LPoint
  , _mLine_end :: Dynamic t LPoint
}

data MText t = MText {
  _mText_box    :: Dynamic t LBox
  , _mText_text :: Dynamic t Text
}

data MRelBox t = MRelBox {
  _mRelBox_original :: Dynamic t LBox
  , _mRelBox_box    :: Dynamic t LBox
}

type Manipulator t = DS.DSum (MTag t) Identity

data CBox = CBox {
  _cBox_box :: DeltaLBox
}

data CLine = CLine {
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
  -- TODO manual instances needed
  --deriving anyclass Data.GADT.Compare.GEq
  --deriving anyclass DM.GCompare

type Controller = DS.DSum CTag Identity
