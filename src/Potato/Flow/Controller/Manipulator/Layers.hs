{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Layers (
  LayersHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Layers
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq

data LayersHandler = LayersHandler {
    _layersHandler_isActive   :: Bool
  }

instance Default LayersHandler where
  def = LayersHandler {
      _layersHandler_isActive = False
    }


instance PotatoHandler LayersHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse lh@LayersHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Down -> r where
      r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_isActive = True
            }
        }
    MouseDragState_Dragging -> Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh
      }
    MouseDragState_Up -> r where
      r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_isActive = False
            }
        }
    _ -> error "unexpected mouse state passed to handler"
  pHandleKeyboard _ _ _ = Nothing
  pHandleCancel slh _ = def {
      _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler def {
          _layersHandler_isActive = False
        }
    }
  pRenderHandler slh PotatoHandlerInput {..} = HandlerRenderOutput
  pIsHandlerActive = _layersHandler_isActive
