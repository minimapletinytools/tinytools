{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Pan (
  PanHandler(..)
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

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Sequence                  as Seq

data PanHandler = PanHandler {
    _panHandler_pan :: XY
  }

instance Default PanHandler where
  def = PanHandler { _panHandler_pan = 0 }

instance PotatoHandler PanHandler where
  pHandlerName _ = handlerName_pan
  pHandleMouse ph@PanHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag md@MouseDrag {..}) = Just $ case _mouseDrag_state of
    MouseDragState_Cancelled -> error "unexpected mouse state passed to handler"
    MouseDragState_Down -> def { _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler ph }
    _ -> def {
        _potatoHandlerOutput_nextHandler = case _mouseDrag_state of
          MouseDragState_Dragging -> Just $ SomePotatoHandler ph { _panHandler_pan = delta }
          MouseDragState_Up -> Nothing
        , _potatoHandlerOutput_pan = Just (delta - _panHandler_pan)
      } where delta = _mouseDrag_to - _mouseDrag_from

  pHandleKeyboard _ PotatoHandlerInput {..} kbd = Nothing
  pHandleCancel PanHandler {..} PotatoHandlerInput {..} = def { _potatoHandlerOutput_pan = Just $ - _panHandler_pan }
  pRenderHandler _ PotatoHandlerInput {..} = HandlerRenderOutput
  pIsHandlerActive _ = True
