{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  SimpleLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Default
import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM


data SimpleLineHandler = SimpleLineHandler {
    _simpleLineHandler_isStart     :: Bool --either we are manipulating start, or we are manipulating end
    , _simpleLineHandler_undoFirst :: Bool
  }

instance Default SimpleLineHandler where
  def = SimpleLineHandler {
      _simpleLineHandler_isStart = False
      , _simpleLineHandler_undoFirst = False
    }

--handleMouse :: Maybe SimpleLineHandler -> PFState -> Selection -> RelMouseDrag -> PotatoHandlerOutput
--handleMouse mslh pfs sel rmd = case

instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@SimpleLineHandler {..} PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Dragging -> Just r where
      op = Nothing -- TODO
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh
          , _potatoHandlerOutput_event = op
        }
    MouseDragState_Up -> Just def
    _ -> error "unexpected mouse state passed to handler"
  pHandleKeyboard _ _ _ = Nothing
  pHandleCancel slh _ = if pIsHandlerActive slh
    -- TODO enable this when we are done implementing
    --then def { _potatoHandlerOutput_event = Just PFEUndo }
    then def
    else def
  pRenderHandler slh PotatoHandlerInput {..} = HandlerRenderOutput
  -- if undoFirst is true then we have already started dragging
  pIsHandlerActive = _simpleLineHandler_undoFirst
