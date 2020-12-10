{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (

) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM


data SimpleLineHandler = SimpleLineHandler {
    _simpleLineHandler_isStart :: Bool --either we are manipulating start, or we are manipulating end
  }

--handleMouse :: Maybe SimpleLineHandler -> PFState -> Selection -> RelMouseDrag -> PotatoHandlerOutput
--handleMouse mslh pfs sel rmd = case

instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = "SimpleLineHandler"
  pHandleMouse slh@SimpleLineHandler {..} pfs sel (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Dragging -> Just (Just (SomePotatoHandler slh), Nothing, op) where
      op = Nothing -- TODO
    MouseDragState_Up -> Just (Nothing, Nothing, Nothing)
    _ -> error "unexpected mouse state passed to handler"
  pHandleKeyboard _ _ _ _ = Nothing
  pValidateMouse _ (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Down      -> False
    MouseDragState_Cancelled -> False
    _                        -> True
