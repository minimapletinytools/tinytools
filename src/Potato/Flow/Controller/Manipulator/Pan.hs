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

import           Data.Default
import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Sequence                  as Seq


data PanHandler = PanHandler

instance Default PanHandler where
  def = PanHandler

instance PotatoHandler PanHandler where
  pHandlerName _ = "PanHandler"
  pHandleMouse ph PotatoHandlerInput {..} rmd@(RelMouseDrag md) = Just $ case _mouseDrag_state md of
    _ -> (Just $ SomePotatoHandler ph, Nothing, Nothing)

    -- TODO pass on new pan info
    MouseDragState_Up -> (Nothing, Nothing, Nothing)

    MouseDragState_Cancelled -> error "unexpected mouse state passed to handler"
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = Nothing
  pHandleCancel sh PotatoHandlerInput {..} = (Nothing, Nothing, Nothing)
  pRenderHandler sh PotatoHandlerInput {..} = HandlerRenderOutput
  pIsHandlerActive _ = True
