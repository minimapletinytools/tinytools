{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.New.State (
  PFState(..)
  , pFState_maxID
  , emptyPFState
  , do_newElts
  , undo_newElts
  , do_deleteElts
  , undo_deleteElts
  , do_resizeCanvas
  , undo_resizeCanvas
  , do_manipulate
) where

import           Relude


import           Potato.Flow.Math
import           Potato.Flow.New.Cmd
import           Potato.Flow.New.Layers
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts
import Potato.Flow.Reflex.RElts

import           Reflex

import           Data.Aeson
import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict       as IM
import qualified Data.List.NonEmpty       as NE
import qualified Data.List.Safe           as SL
import qualified Data.Sequence            as Seq


data PFState = PFState {
  _pFState_layers      :: Seq REltId
  -- TODO cache REltId -> Layers map with dirty flag probably
  , _pFState_directory :: IM.IntMap SEltLabel
  , _pFState_canvas    :: LBox
} deriving (Show, Generic)

instance FromJSON PFState
instance ToJSON PFState
instance NFData PFState

pFState_maxID :: PFState -> REltId
pFState_maxID s = maybe 0 fst (IM.lookupMax (_pFState_directory s))

emptyPFState :: PFState
emptyPFState = PFState Seq.empty IM.empty (LBox 0 0)

do_newElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
do_newElts seltls' PFState {..} = r where
  seltls = toList seltls'
  poss = map (\(x,y,_) -> (y,x)) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  newLayers = insertEltList poss _pFState_layers
  newDir = IM.fromList els `IM.union` _pFState_directory
  r = PFState newLayers newDir _pFState_canvas

undo_newElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
undo_newElts seltls' PFState {..} = r where
  seltls = toList seltls'
  poss = map (\(_,y,_) -> y) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  newLayers = removeEltList poss _pFState_layers
  newDir = _pFState_directory `IM.difference` fromList els
  r = PFState newLayers newDir _pFState_canvas

do_deleteElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
do_deleteElts = undo_newElts

undo_deleteElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
undo_deleteElts = do_newElts

do_resizeCanvas :: DeltaLBox -> PFState -> PFState
do_resizeCanvas d pfs = pfs { _pFState_canvas = plusDelta (_pFState_canvas pfs) d }

undo_resizeCanvas :: DeltaLBox -> PFState -> PFState
undo_resizeCanvas d pfs = pfs { _pFState_canvas = minusDelta (_pFState_canvas pfs) d }

-- TODO
manipulate :: Bool -> ControllersWithId -> PFState -> PFState
manipulate isDo cs pfs = r where
  dir = _pFState_directory pfs
  difffn _ seltl c = Just $ updateFnFromController isDo c seltl
  newDir = IM.differenceWithKey difffn dir cs
  r = pfs { _pFState_directory = newDir }

do_manipulate :: ControllersWithId -> PFState -> PFState
do_manipulate = manipulate True

undo_manipulate :: ControllersWithId -> PFState -> PFState
undo_manipulate = manipulate False
