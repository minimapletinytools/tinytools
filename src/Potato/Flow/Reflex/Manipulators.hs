{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}

module Potato.Flow.Reflex.Manipulators (
  MNoneView
  , MBoxView(..)
  , MLineView(..)
  , MTextView(..)
  , MViewTag(..)
  , MViewSum

  {-
  , MNoneControl
  , MBoxControl(..)
  , MLineControl(..)
  , MControlTag(..)
  , MControlCmd
  -}
) where

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
-- manipEv = dyn manipulator :: m (Event t (ManipulatorAction t))
-- pass pack to PFC
-- rec _pfc_manipulate = manipEv



--types
--REltLabelt -> (REltId, DSum MViewTag Identity)
-- lib must know to map DSum MViewTag Identity to DSum MControlTag Identity


data MViewTag t a where
  MViewTagNone :: MViewTag t MNoneView
  MViewTagBox :: MViewTag t (MBoxView t)
  MViewTagLine :: MViewTag t (MLineView t)
  MViewTagText :: MViewTag t (MTextView t)
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare


type MNoneView = ()


data MBoxView t = MBoxView {
  _mBoxView_box :: Dynamic t LBox
}

data MLineView t = MLineView {
  _mLineView_start :: Dynamic t LPoint
  , _mLineView_end :: Dynamic t LPoint
}


data MTextView t = MTextView {
  mTextView_box    :: Dynamic t LBox
  , mTextView_text :: Dynamic t Text
}

type MViewSum t = DS.DSum (MViewTag t) Identity

{-

data MControlTag t a where
  MControlTagNone :: MControlTag t MNoneView
  MControlTagBox :: MControlTag t (MBoxView t)
  MControlTagLine :: MControlTag t (MLineView t)
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare

data MLineControl t = MLineControl {
  lineStartControl :: Dynamic t LPoint
  , lineEndControl :: Dynamic t LPoint
}
-- TODO helper to create MBoxControl from dragging various parts of the manipulator
data MBoxControl t = MBoxControl {
  boxControl :: Event t LBox
}
type MNoneControl = ()
type MControlCmd t = DS.DSum (MControlTag t) Identity
-}
