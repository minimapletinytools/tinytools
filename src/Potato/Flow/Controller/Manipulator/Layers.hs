{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Layers (
  LayersHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Owl
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.Types
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.OwlState

import           Data.Default
import qualified Data.IntMap                    as IM
import qualified Data.Sequence                  as Seq
import           Data.Tuple.Extra


data LayerDragState = LDS_None | LDS_Dragging | LDS_Selecting LayerEntryPos deriving (Show, Eq)

data LayerDownType = LDT_Hide | LDT_Lock | LDT_Collapse | LDT_Normal deriving (Show, Eq)


-- TODO we could probably change this to do a more efficient binary search based on position in hierarchy
doesSelectionContainREltId :: REltId -> Selection -> Bool
doesSelectionContainREltId rid = isJust . find (\sowl -> rid == _superOwl_id sowl) . unSuperOwlParliament

-- TODO a little weird to be returning the SuperOwl you clicked on but whatever...
clickLayerNew :: Seq LayerEntry -> XY -> Maybe (SuperOwl, LayerDownType, Int)
clickLayerNew lentries  (V2 absx lepos) = case Seq.lookup lepos lentries of
  Nothing                      -> Nothing
  Just le -> Just . (,,absx - layerEntry_depth le) sowl $ case () of
    () | layerEntry_isFolder le && layerEntry_depth le == absx -> LDT_Collapse
    () | layerEntry_depth le + 1 == absx   -> LDT_Hide
    () | layerEntry_depth le + 2 == absx -> LDT_Lock
    () -> LDT_Normal
    where
      sowl = _layerEntry_superOwl le


data LayersHandler = LayersHandler {
    _layersHandler_dragState   :: LayerDragState
    , _layersHandler_cursorPos :: XY

  }

instance Default LayersHandler where
  def = LayersHandler {
      _layersHandler_dragState = LDS_None
      , _layersHandler_cursorPos = 0
    }


instance PotatoHandler LayersHandler where
  pHandlerName _ = handlerName_simpleLine

  -- we incorrectly reuse RelMouseDrag for LayersHandler even though LayersHandler doesn't care about canvas pan coords
  -- pan offset should always be set to 0 in RelMouseDrag
  pHandleMouse lh@LayersHandler {..} PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = let
    leposxy@(V2 _ lepos) = _mouseDrag_to
    selection = _potatoHandlerInput_selection
    (LayersState lmm lentries) = _potatoHandlerInput_layersState
    pfs = _potatoHandlerInput_pFState
    in case (_mouseDrag_state, _layersHandler_dragState) of
      (MouseDragState_Down, LDS_None) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        (nextDragState, mNextLayerState, changes) = case clickLayerNew lentries leposxy of
          Nothing -> (LDS_None, Nothing, IM.empty)
          -- (you can only click + drag selected elements)
          Just (downsowl, ldtdown, offset) -> case ldtdown of
            LDT_Normal -> if shift || (not $ doesSelectionContainREltId (_superOwl_id downsowl) selection)
              -- if element wasn't selected or shift is held down, enter selection mode
              then (LDS_Selecting lepos, Nothing, IM.empty)
              else (LDS_Dragging, Nothing, IM.empty)
            LDT_Hide -> (LDS_None, Just $ toggleLayerEntry pfs (LayersState lmm lentries) lepos LHCO_ToggleHide, IM.empty)
            LDT_Lock -> r' where
              nextLayersState = toggleLayerEntry pfs (LayersState lmm lentries) lepos LHCO_ToggleLock
              hideChanges = changesFromToggleHide pfs nextLayersState lepos
              r' = (LDS_None, Just $ nextLayersState, hideChanges)
            LDT_Collapse -> (LDS_None, Just $ toggleLayerEntry pfs (LayersState lmm lentries) lepos LHCO_ToggleCollapse, IM.empty)

        -- TODO also return visual changes
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = nextDragState
                , _layersHandler_cursorPos = _mouseDrag_to
              }
            , _potatoHandlerOutput_layersState = mNextLayerState
          }
      (MouseDragState_Down, _) -> error "unexpected, _layersHandler_dragState should have been reset on last mouse up"
      (MouseDragState_Dragging, LDS_Dragging) -> r where
        -- TODO figure out where we are about to drop and render dummy thingy

        V2 _ lastCursorY = _layersHandler_cursorPos
        V2 _ cursorY = _mouseDrag_to
        goingDown = lastCursorY > cursorY

        --mev = case clickLayerNew lentries leposxy of

        dropIndex' = if goingDown
          then undefined -- child if possibe otherwise sibling
          else undefined -- sibling
        dropIndex = dropIndex' -- TODO clamp

        r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_cursorPos = _mouseDrag_to
            }
        }

      -- TODO somday do drag for multi-select here
      (MouseDragState_Dragging, _) -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_cursorPos = _mouseDrag_to
            }
        }

      -- TODO if mouse didn't move from lposdown, enter renaming mode (new handler I guess)
      (MouseDragState_Up, LDS_Selecting leposdown) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        sowl = _layerEntry_superOwl $ Seq.index lentries leposdown
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = LDS_None
              }
            , _potatoHandlerOutput_select = Just (shift, SuperOwlParliament $ Seq.singleton sowl)
          }
      (MouseDragState_Up, LDS_Dragging) -> r where
        mev = case clickLayerNew lentries leposxy of
          -- release where there is no element, do nothing
          Nothing -> Nothing
          Just (sowl,_,_) -> case doesSelectionContainREltId (_superOwl_id sowl) selection of
            -- dropping on a selected element does onthing
            True  ->  Nothing
            False -> Just $ WSEMoveElt (spot, superOwlParliament_toOwlParliament selection)
            where
              spot = owlTree_owlEltMeta_toOwlSpot (_owlPFState_owlTree _potatoHandlerInput_pFState) (_superOwl_meta sowl)
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = LDS_None
              }
            , _potatoHandlerOutput_pFEvent = mev
          }
      (MouseDragState_Up, LDS_None) -> Just $ setHandlerOnly lh
      (MouseDragState_Cancelled, _) -> Just $ setHandlerOnly lh {
          _layersHandler_dragState = LDS_None
        }
      _ -> error $ "unexpected mouse state passed to handler " <> show _mouseDrag_state <> " " <> show _layersHandler_dragState

  pHandleKeyboard lh@LayersHandler {..} PotatoHandlerInput {..} kbd = case kbd of
    KeyboardData (KeyboardKey_Scroll _) _ -> Nothing -- TODO scrolling
    _ -> Nothing




  pRenderHandler lh@LayersHandler {..} PotatoHandlerInput {..} = if pIsHandlerActive lh
    then HandlerRenderOutput [LBox _layersHandler_cursorPos (V2 1 1)]
    else emptyHandlerRenderOutput
  pIsHandlerActive LayersHandler {..} = _layersHandler_dragState /= LDS_None
