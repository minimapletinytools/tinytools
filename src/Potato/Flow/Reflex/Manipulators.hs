{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}

module Potato.Flow.Reflex.Manipulators (
  MBoxView(..)
  , MBoxControl(..)
  , MLineView(..)
  , MLineControl(..)
  , PFMViewSum
  , PFMControlCmd
) where

import           Relude

import           Reflex

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Manipulators.Tags

import qualified Data.Dependent.Sum                   as DS

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



data MBoxView t = MBoxView {
  boxView :: Dynamic t LBox
}
-- TODO helper to create MBoxControl from dragging various parts of the manipulator
data MBoxControl t = MBoxControl {
  boxControl :: Event t LBox
}

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


type PFMViewSum t = DS.DSum MTag (PFMView t)
type PFMControlCmd t = DS.DSum MTag (PFMControl t)
