{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Select (
  SelectHandler(..)
  , selectMagic
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types
import           Potato.Flow.Workspace

import           Data.Default
import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Sequence                  as Seq



-- TODO ignore locked elements
-- NOTE hidden stuff is already removed from BroadPhaseState
selectMagic :: PFState -> LayerPosMap -> BroadPhaseState -> RelMouseDrag -> Selection
selectMagic pFState layerPosMap bps (RelMouseDrag MouseDrag {..}) = r where
  LBox pos' sz' = make_lBox_from_XYs _mouseDrag_to _mouseDrag_from
  -- always expand selection by 1
  selectBox = LBox pos' (sz' + V2 1 1)
  boxSize = lBox_area selectBox
  singleClick = boxSize == 1

  isboxshaped = \case
    SEltLabel _ (SEltBox _) -> True
    SEltLabel _ (SEltTextArea _) -> True
    _ -> False

  unculledRids = broadPhase_cull_includeZero selectBox (_broadPhaseState_bPTree bps)
  selectedRids = flip filter unculledRids $ \rid -> case IM.lookup rid (_pFState_directory pFState) of
    Nothing -> error $ "expected to find rid in directory " <> show rid
    -- if it's box shaped, there's no need to test doesSEltIntersectBox as we already know it intersects
    Just seltl | isboxshaped seltl -> True
    Just (SEltLabel _ selt) -> doesSEltIntersectBox selectBox selt

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

data SelectHandler = SelectHandler {
    _selectHandler_selectArea :: LBox
  }

instance Default SelectHandler where
  def = SelectHandler {
      _selectHandler_selectArea = LBox 0 0
    }

instance PotatoHandler SelectHandler where
  pHandlerName _ = handlerName_select
  pHandleMouse sh PotatoHandlerInput {..} rmd@(RelMouseDrag md) = Just $ case _mouseDrag_state md of
    MouseDragState_Down -> captureWithNoChange sh
    MouseDragState_Dragging -> captureWithNoChange sh
    MouseDragState_Up -> def { _potatoHandlerOutput_select = Just (shiftClick, newSelection) }  where
      shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers md)
      newSelection = selectMagic _potatoHandlerInput_pFState _potatoHandlerInput_layerPosMap _potatoHandlerInput_broadPhase rmd
    MouseDragState_Cancelled -> def
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = Nothing
  pRenderHandler sh PotatoHandlerInput {..} = HandlerRenderOutput [_selectHandler_selectArea sh]
  pIsHandlerActive _ = True
