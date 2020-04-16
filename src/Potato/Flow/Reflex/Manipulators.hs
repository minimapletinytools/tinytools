{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Potato.Flow.Reflex.Manipulators (
  MBoxView(..)
  , MBoxControl(..)
  , MLineView(..)
  , MLineControl(..)
  , PFMViewCmd
  , PFMControlCmd
) where

import           Relude

import Reflex

import Potato.Flow.Math

import qualified Data.GADT.Compare
import qualified Data.Dependent.Map              as DM
import qualified Data.Dependent.Sum              as DS

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


data MBox
data MBoxView t = MBoxView {
  boxView :: Dynamic t LBox
}
-- TODO helper to create MBoxControl from dragging various parts of the manipulator
data MBoxControl t = MBoxControl {
  boxControl :: Event t LBox
}
data MLine
data MLineView t = MLineView {
  lineStartView :: Dynamic t LPoint
  , lineEndView :: Dynamic t LPoint
}
data MLineControl t = MLineControl {
  lineStartControl :: Dynamic t LPoint
  , lineEndControl :: Dynamic t LPoint
}

type family PFMView a :: Type -> Type where
  PFMView MBox = MBoxView
  PFMView MLine = MLineView

type family PFMControl a :: Type -> Type where
  PFMControl MBox = MBoxControl
  PFMControl MLine = MLineControl

data PFMTag a where
  PFMTagBox :: PFMTag MBox
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare


type PFMViewCmd t = DS.DSum PFMTag (PFMView t)
type PFMControlCmd t = DS.DSum PFMTag (PFMControl t)
