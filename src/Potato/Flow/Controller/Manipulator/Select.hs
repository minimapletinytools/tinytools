{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Select (
  SelectHandler(..)
  , selectMagic
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Data.Default
import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Sequence                  as Seq

selectMagic :: PFState -> REltIdMap LayerPos -> BroadPhaseState -> RelMouseDrag -> Selection
selectMagic pFState layerPosMap bps (RelMouseDrag MouseDrag {..}) = r where
  LBox pos' sz' = make_LBox_from_XYs _mouseDrag_to _mouseDrag_from
  -- always expand selection by 1
  selectBox = LBox pos' (sz' + V2 1 1)
  boxSize = lBox_area selectBox
  singleClick = boxSize == 1
  selectedRids = broadPhase_cull selectBox (_broadPhaseState_bPTree bps)
  mapToLp = map (\rid -> (fromJust . IM.lookup rid $ layerPosMap))
  lps' = mapToLp selectedRids
  lps = if singleClick
    -- single click, select top elt only
    then case lps' of
      [] -> []
      xs -> [L.maximumBy (\lp1 lp2 -> compare lp2 lp1) xs]
    -- otherwise select everything
    else lps'
  r = Seq.fromList $ map (pfState_layerPos_to_superSEltLabel pFState) lps

-- TODO move to another file?
data SelectHandler = SelectHandler {
    _selectHandler_selecting :: Bool
  }

instance Default SelectHandler where
  def = SelectHandler {
      _selectHandler_selecting = False
    }

instance PotatoHandler SelectHandler where
  pHandlerName _ = "SelectHandler"
  pHandleMouse sh PotatoHandlerInput {..} rmd@(RelMouseDrag md) = Just $ case _mouseDrag_state md of
    MouseDragState_Down -> def { _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler sh { _selectHandler_selecting = True} }
    MouseDragState_Dragging -> def { _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler sh }
    MouseDragState_Up -> def { _potatoHandlerOutput_select = traceShowId $ Just (shiftClick, newSelection) }  where
      shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers md)
      newSelection = selectMagic _potatoHandlerInput_pFState _potatoHandlerInput_layerPosMap _potatoHandlerInput_broadPhase rmd
    MouseDragState_Cancelled -> error "unexpected mouse state passed to handler"
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = Nothing
  pHandleCancel sh PotatoHandlerInput {..} = def
  pRenderHandler sh PotatoHandlerInput {..} = HandlerRenderOutput
  -- same as default?
  --pValidateMouse sh (RelMouseDrag MouseDrag {..}) = if _selectHandler_selecting sh
  --  then _mouseDrag_state /= MouseDragState_Down
  --  else _mouseDrag_state == MouseDragState_Down
  pIsHandlerActive = _selectHandler_selecting
