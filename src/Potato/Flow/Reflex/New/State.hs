{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE RecursiveDo             #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Potato.Flow.Reflex.New.State (
) where

import           Relude


import           Potato.Flow.Reflex.New.Cmd
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Reflex

import           Data.Dependent.Sum         (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict         as IM

type ActionStack t = Seq (PFCmd t)

-- TODO serialize only PFState and REltId part?
data PFWorkspace t = PFWorkspace {
  state         :: PFState
  , actionStack :: ActionStack t
  , maxId       :: REltId
}

data PFState = PFState {
  layers      :: Seq REltId
  -- TODO cache REltId -> Layers map with dirty flag probably
  , directory :: IM.IntMap SEltLabel
}


{-
data PFCmdTag t a where
  -- LayerPos indices are as if all elements already exist in the map
  PFCNewElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  -- LayerPos indices are the current indices of elements to be removed
  PFCDeleteElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  --PFCMove :: PFCmdTag t (LayerPos, NonEmpty LayerPos)
  --PFCPaste :: PFCmdTag t (LayerPos, [REltId, SEltLabel])
  --PFCDuplicate :: PFCmdTag t [REltId]
  PFCManipulate :: PFCmdTag t (ControllersWithId)
  PFCResizeCanvas :: PFCmdTag t DeltaLBox
-}

-- TODO you probably want to output something like this as well
--_sEltLayerTree_changeView :: REltIdMap (MaybeMaybe SEltLabel)

doCmd :: PFCmd t -> PFState -> PFState
doCmd cmd state = case cmd of
  (PFCNewElts :=> Identity x) ->  doNewElts state
  _                           -> undefined

undoCmd :: PFCmd t -> PFState -> PFState
undoCmd cmd state = case cmd of
  (PFCNewElts :=> Identity x) ->  undoNewElts state
  _                           -> undefined

doNewElts :: PFState -> PFState
doNewElts state = undefined

undoNewElts :: PFState -> PFState
undoNewElts state = undefined
