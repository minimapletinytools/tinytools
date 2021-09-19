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


data TextAreaHandler = TextAreaHandler {
    _textAreaHandler_active      :: Bool
  }


-- TODO FINISH
instance PotatoHandler TextAreaHandler where
  pHandlerName _ = handlerName_textArea
  pHandlerDebugShow TextAreaHandler {..} = ""
  pHandleMouse tah PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = Nothing
  pHandleKeyboard tah PotatoHandlerInput {..} (KeyboardData k _) = Nothing
  pResetHandler tah PotatoHandlerInput {..} = Nothing
  pRenderHandler tah' PotatoHandlerInput {..} = HandlerRenderOutput $ []
  pIsHandlerActive = _textAreaHandler_active
