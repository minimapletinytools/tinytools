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
import           Potato.Flow.OwlState
import           Potato.Flow.Owl

import           Data.Default
import Data.Foldable (maximumBy)
import qualified Data.Sequence                  as Seq



-- TODO ignore locked elements
-- NOTE hidden stuff is already removed from BroadPhaseState
selectMagic :: OwlPFState -> BroadPhaseState -> RelMouseDrag -> Selection
selectMagic pfs bps (RelMouseDrag MouseDrag {..}) = r where
  LBox pos' sz' = make_lBox_from_XYs _mouseDrag_to _mouseDrag_from
  -- always expand selection by 1
  selectBox = LBox pos' (sz' + V2 1 1)
  boxSize = lBox_area selectBox
  singleClick = boxSize == 1

  isboxshaped sowl = case _superOwl_elt sowl of
    OwlEltSElt _ (SEltBox _)  -> True
    OwlEltSElt _ (SEltTextArea _) -> True
    _ -> False

  unculledrids = broadPhase_cull_includeZero selectBox (_broadPhaseState_bPTree bps)
  unculledsowls = fmap (\rid ->  owlTree_mustFindSuperOwl (_owlPFState_owlTree pfs) rid) unculledrids
  selectedsowls' = flip filter unculledsowls $ \case
    -- if it's box shaped, there's no need to test doesSEltIntersectBox as we already know it intersects
    sowl | isboxshaped sowl -> True
    sowl -> doesSEltIntersectBox selectBox (isOwl_toSElt_hack sowl)


  -- TODO consider using makeSortedSuperOwlParliament instead (prob a little faster?)
  selectedsowls = if singleClick
    -- single click, select top elt only
    then case selectedsowls' of
      [] -> []
      _ ->  [maximumBy (\s1 s2 -> owlTree_superOwl_comparePosition (_owlPFState_owlTree pfs) s2 s1) selectedsowls']
    -- otherwise select everything
    else selectedsowls'
  r = SuperOwlParliament $ Seq.fromList selectedsowls

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
      newSelection = selectMagic _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase rmd
    MouseDragState_Cancelled -> def
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = Nothing
  pRenderHandler sh PotatoHandlerInput {..} = HandlerRenderOutput [defaultRenderHandle $ _selectHandler_selectArea sh]
  pIsHandlerActive _ = True
