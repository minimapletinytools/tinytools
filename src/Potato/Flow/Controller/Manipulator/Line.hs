{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  SimpleLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.OwlState
import           Potato.Flow.Owl

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq


data SimpleLineHandler = SimpleLineHandler {
    _simpleLineHandler_isStart      :: Bool --either we are manipulating start, or we are manipulating end
    , _simpleLineHandler_undoFirst  :: Bool
    , _simpleLineHandler_isCreation :: Bool
    , _simpleLineHandler_active     :: Bool
  } deriving (Show)

instance Default SimpleLineHandler where
  def = SimpleLineHandler {
      _simpleLineHandler_isStart = False
      , _simpleLineHandler_undoFirst = False
      , _simpleLineHandler_isCreation = False
      , _simpleLineHandler_active = False
    }


findFirstLineManipulator :: RelMouseDrag -> CanvasSelection -> Maybe Bool
findFirstLineManipulator (RelMouseDrag MouseDrag {..}) (CanvasSelection selection) = assert (Seq.length selection == 1) $ r where
  msowl = Seq.lookup 0 selection
  selt = case msowl of
    Nothing -> error "expected selection"
    Just sowl -> superOwl_toSElt_hack sowl
  r = case selt of
    SEltLine SSimpleLine {..} ->
      if _mouseDrag_to == _sSimpleLine_start then Just True
        else if _mouseDrag_to == _sSimpleLine_end then Just False
          else Nothing
    x -> error $ "expected SSimpleLine in selection but got " <> show x <> " instead"


instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@SimpleLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of

    MouseDragState_Down | _simpleLineHandler_isCreation -> Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
            _simpleLineHandler_active = True
          }
      }
    -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
    -- TODO consider moving this into GoatWidget since it's needed by many manipulators
    MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
    MouseDragState_Down -> r where
      mistart = findFirstLineManipulator rmd _potatoHandlerInput_canvasSelection
      r = case mistart of
        Nothing -> Nothing -- did not click on manipulator, no capture
        Just isstart -> Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _simpleLineHandler_isStart = isstart
                , _simpleLineHandler_active = True
              }
          }
    MouseDragState_Dragging -> Just r where
      -- TODO handle shift click using restrict8
      dragDelta = _mouseDrag_to - _mouseDrag_from

      -- for creating new elt
      newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection

      -- for manipulation
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
      rid = _superOwl_id sowl
      controller = CTagLine :=> (Identity $ if _simpleLineHandler_isStart
        then def { _cLine_deltaStart = Just $ DeltaXY dragDelta }
        else def { _cLine_deltaEnd = Just $ DeltaXY dragDelta })

      op = if _simpleLineHandler_isCreation
        then WSEAddElt (_simpleLineHandler_undoFirst, newEltPos, OwlEltSElt (OwlInfo "<line>") $ SEltLine $ SSimpleLine _mouseDrag_from _mouseDrag_to def def)
        else WSEManipulate (_simpleLineHandler_undoFirst, IM.singleton rid controller)

      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
              _simpleLineHandler_undoFirst = True
            }
          , _potatoHandlerOutput_pFEvent = Just op
        }
    MouseDragState_Up -> Just def
    MouseDragState_Cancelled -> Just def
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler slh@SimpleLineHandler {..} PotatoHandlerInput {..} = r where
    mselt = selectionToMaybeSuperOwl _potatoHandlerInput_canvasSelection >>= return . superOwl_toSElt_hack
    boxes = case mselt of
      Just (SEltLine SSimpleLine {..}) -> if _simpleLineHandler_active
        -- TODO if active, color selected handler
        then [make_lBox_from_XY _sSimpleLine_start, make_lBox_from_XY _sSimpleLine_end]
        else [make_lBox_from_XY _sSimpleLine_start, make_lBox_from_XY _sSimpleLine_end]
      _ -> []
    r = HandlerRenderOutput (fmap defaultRenderHandle boxes)
  pIsHandlerActive = _simpleLineHandler_active
