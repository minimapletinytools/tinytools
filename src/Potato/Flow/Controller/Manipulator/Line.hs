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
    _simpleLineHandler_isStart :: Bool --either we are manipulating start, or we are manipulating end
  }

instance Default SimpleLineHandler where
  def = SimpleLineHandler {
      _simpleLineHandler_isStart = False
    }

--handleMouse :: Maybe SimpleLineHandler -> PFState -> Selection -> RelMouseDrag -> PotatoHandlerOutput
--handleMouse mslh pfs sel rmd = case

instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = "SimpleLineHandler"
  pHandleMouse slh@SimpleLineHandler {..} PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Dragging -> Just (Just (SomePotatoHandler slh), Nothing, op) where
      op = Nothing -- TODO
    MouseDragState_Up -> Just (Nothing, Nothing, Nothing)
    _ -> error "unexpected mouse state passed to handler"
  pHandleKeyboard _ _ _ = Nothing
  pHandleCancel _ _ = (Nothing, Nothing, Nothing)
  pRenderHandler slh PotatoHandlerInput {..} = HandlerRenderOutput
  pValidateMouse _ (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Down      -> False
    MouseDragState_Cancelled -> False
    _                        -> True
