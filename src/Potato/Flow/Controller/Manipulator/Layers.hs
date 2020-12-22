{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Layers (
  LayersHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Layers
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq
import           Data.Tuple.Extra

data LayersHandler = LayersHandler {
    _layersHandler_dragState      :: LayerDragState
    , _layersHandler_absCursorPos :: XY
  }

instance Default LayersHandler where
  def = LayersHandler {
      _layersHandler_dragState = LDS_None
      , _layersHandler_absCursorPos = 0
    }


instance PotatoHandler LayersHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse lh@LayersHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
    abspos = _mouseDrag_to
    leposxy@(V2 _ lepos) = _mouseDrag_to + (V2 0 _potatoHandlerInput_layerScrollPos)
    selection = _potatoHandlerInput_selection
    (lmm, lentries) = _potatoHandlerInput_layersState
    pfs = _potatoHandlerInput_pFState
    in case (_mouseDrag_state, _layersHandler_dragState) of
      (MouseDragState_Down, LDS_None) -> r where

        (nextDragState, mNextLayerState) = case clickLayerNew selection lentries leposxy of
          Nothing -> (LDS_None, Nothing)
          -- (you can only click + drag selected elements)
          Just (downlp, ldtdown) -> case ldtdown of
            LDT_Normal -> case doesSelectionContainLayerPos downlp selection of
              False -> (LDS_Selecting lepos, Nothing)
              True  -> (LDS_Dragging, Nothing)
            LDT_Hide -> (LDS_None, Just $ toggleLayerEntry pfs lmm lentries lepos LHCO_ToggleHide)
            LDT_Lock -> (LDS_None, Just $ toggleLayerEntry pfs lmm lentries lepos LHCO_ToggleLock)
            LDT_Collapse -> (LDS_None, Just $ toggleLayerEntry pfs lmm lentries lepos LHCO_ToggleCollapse)

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = nextDragState
                , _layersHandler_absCursorPos = abspos
              }
            , _potatoHandlerOutput_layersState = mNextLayerState
          }
      (MouseDragState_Down, _) -> error "unexpected, _layersHandler_dragState should have been reset on last mouse up"
      (MouseDragState_Dragging, _) -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_absCursorPos = abspos
            }
        }
      (MouseDragState_Up, LDS_Selecting leposdown) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        sel = _layerEntry_superSEltLabel $ Seq.index lentries leposdown
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = LDS_None
              }
            , _potatoHandlerOutput_select = Just (shift, Seq.singleton sel)
          }
      (MouseDragState_Up, LDS_Dragging) -> r where
        mev = case clickLayerNew selection lentries leposxy of
          -- release where there is no element, do nothing
          Nothing -> Nothing
          Just (uplp,_) -> case doesSelectionContainLayerPos uplp selection of
            -- dropping on a selected element does onthing
            True  ->  Nothing
            False -> Just $ PFEMoveElt (toList (fmap snd3 selection), uplp)
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = LDS_None
              }
            , _potatoHandlerOutput_pFEvent = mev
          }
      _ -> error "unexpected mouse state passed to handler"
  pHandleKeyboard _ _ _ = Nothing
  pHandleCancel slh _ = def {
      _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler def {
          _layersHandler_dragState = LDS_None
        }
    }
  pRenderHandler lh@LayersHandler {..} PotatoHandlerInput {..} = if pIsHandlerActive lh
    then HandlerRenderOutput [LBox _layersHandler_absCursorPos (V2 1 1)]
    else emptyHandlerRenderOutput
  pIsHandlerActive lh@LayersHandler {..} = _layersHandler_dragState /= LDS_None
