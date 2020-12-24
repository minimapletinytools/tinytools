{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  SimpleLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

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
  }

instance Default SimpleLineHandler where
  def = SimpleLineHandler {
      _simpleLineHandler_isStart = False
      , _simpleLineHandler_undoFirst = False
      , _simpleLineHandler_isCreation = False
      , _simpleLineHandler_active = False
    }


-- TODO rewrite using selectionToSuperSEltLabel
findFirstLineManipulator :: RelMouseDrag -> Selection -> Maybe Bool
findFirstLineManipulator (RelMouseDrag MouseDrag {..}) selection = assert (Seq.length selection == 1) $ case Seq.lookup 0 selection of
    Nothing -> error "expected selection"
    Just (_, _, SEltLabel _ (SEltLine SLine {..})) ->
      if _mouseDrag_to == _sLine_start then Just True
        else if _mouseDrag_to == _sLine_end then Just False
          else Nothing

    Just x -> error $ "expected SLine in selection but got " <> show x <> " instead"


instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@SimpleLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of

    MouseDragState_Down | _simpleLineHandler_isCreation -> Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
            _simpleLineHandler_active = True
          }
      }
    MouseDragState_Down -> r where
      mistart = findFirstLineManipulator rmd _potatoHandlerInput_selection
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
      newEltPos = lastPositionInSelection _potatoHandlerInput_selection

      -- for manipulation
      (rid, _, _) = selectionToSuperSEltLabel _potatoHandlerInput_selection
      controller = CTagLine :=> (Identity $ if _simpleLineHandler_isStart
        then def { _cLine_deltaStart = Just $ DeltaXY dragDelta }
        else def { _cLine_deltaEnd = Just $ DeltaXY dragDelta })

      op = if _simpleLineHandler_isCreation
        then PFEAddElt (_simpleLineHandler_undoFirst, (newEltPos, SEltLabel "<line>" $ SEltLine $ SLine _mouseDrag_from _mouseDrag_to def))
        else PFEManipulate (_simpleLineHandler_undoFirst, IM.singleton rid controller)

      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh
          , _potatoHandlerOutput_pFEvent = Just op
        }
    MouseDragState_Up -> Just def
    _ -> error "unexpected mouse state passed to handler"
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = case kbd of
    KeyboardData KeyboardKey_Esc _ -> Just $ def
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler SimpleLineHandler {..} PotatoHandlerInput {..} = r where
    boxes = case selectionToSuperSEltLabel _potatoHandlerInput_selection of
      (_,_,SEltLabel _ (SEltLine SLine {..})) -> if _simpleLineHandler_active
        -- TODO if active, color selected handler
        then [make_lBox_from_XY _sLine_start, make_lBox_from_XY _sLine_end]
        else [make_lBox_from_XY _sLine_start, make_lBox_from_XY _sLine_end]
      x -> error $ "expected SLine in selection but got " <> show x <> " instead"
    r = HandlerRenderOutput boxes
  pIsHandlerActive = _simpleLineHandler_active
