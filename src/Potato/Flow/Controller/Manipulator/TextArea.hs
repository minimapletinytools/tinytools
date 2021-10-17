{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextArea (
  TextAreaHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.Owl
import           Potato.Flow.OwlWorkspace

import Data.Default


data TextAreaHandler = TextAreaHandler {
    _textAreaHandler_prevHandler :: SomePotatoHandler
  }

makeTextAreaHandler :: SomePotatoHandler -> TextAreaHandler
makeTextAreaHandler prev = TextAreaHandler {
    _textAreaHandler_prevHandler = prev
  }


-- TODO FINISH
instance PotatoHandler TextAreaHandler where
  pHandlerName _ = handlerName_textArea
  pHandlerDebugShow tah = ""

  -- TODO FINISH
  pHandleMouse tah PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      --(_, sbox) = getSBox _potatoHandlerInput_canvasSelection
      aoeu = undefined
    in
      case _mouseDrag_state of
        MouseDragState_Down -> r where
          r = Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah
            }
        -- TODO "painting" mode someday
        MouseDragState_Dragging -> Just $ captureWithNoChange tah
        MouseDragState_Up -> Just $ captureWithNoChange tah
        MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_textAreaHandler_prevHandler tah) }
    _ -> Nothing
  pResetHandler tah PotatoHandlerInput {..} = Nothing
  pRenderHandler tah PotatoHandlerInput {..} = HandlerRenderOutput $ []
  pIsHandlerActive tah = False
