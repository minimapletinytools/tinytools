{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}

module Potato.Flow.Reflex.Manipulators
  ( MNone
  , MBox(..)
  , MLine(..)
  , MText(..)
  , MTag(..)
  , Manipulators
  , CBox(..)
  , CLine(..)
  , CText(..)
  , CTag(..)
  , Controllers
  )
where

import           Relude

import           Reflex

import           Potato.Flow.Math
import           Potato.Flow.SElts

import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS
import qualified Data.GADT.Compare


-- pattern for piping manipulator back into PFC
-- start with: _pfo_allElts     :: Behavior t (Map REltId (REltLabel t))
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
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare

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

type Manipulators t = DS.DSum (MTag t) Identity

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
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare

type Controllers = DS.DSum CTag Identity
