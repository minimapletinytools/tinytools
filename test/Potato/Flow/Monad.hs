{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Monad (
  GoatWidgetTest
  , runGoatTestApp
) where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Host.Class
import           Reflex.Test.Monad.Host

import           Potato.Flow
import           Potato.Flow.Controller.GoatWidget
import           Potato.Flow.TestStates

import           Control.Monad.Fix

data GoatWidgetTest t (m :: Type -> Type)

instance (TestGuestConstraints t m) => ReflexTestApp (GoatWidgetTest t m) t m where
  data AppInputTriggerRefs (GoatWidgetTest t m) = GoatWidgetTest_InputTriggerRefs {
      _goatWidgetTest_inputTriggerRefs_mouse         :: ReflexTriggerRef t m LMouseData
      , _goatWidgetTest_inputTriggerRefs_keyboard      :: ReflexTriggerRef t m KeyboardData
      , _goatWidgetTest_inputTriggerRefs_selectTool    :: ReflexTriggerRef t m Tool
      , _goatWidgetTest_inputTriggerRefs_load          :: ReflexTriggerRef t m EverythingLoadState
      , _goatWidgetTest_inputTriggerRefs_setDebugLabel :: ReflexTriggerRef t m Text
    }
  data AppInputEvents (GoatWidgetTest t m) = GoatWidgetTest_InputEvents {
      _goatWidgetTest_inputEvents_initialState    :: PFState
      , _goatWidgetTest_inputEvents_mouse         :: Event t LMouseData
      , _goatWidgetTest_inputEvents_keyboard      :: Event t KeyboardData
      , _goatWidgetTest_inputEvents_selectTool    :: Event t Tool
      , _goatWidgetTest_inputEvents_load          :: Event t EverythingLoadState
      , _goatWidgetTest_inputEvents_setDebugLabel :: Event t Text
    }
  data AppOutput (GoatWidgetTest t m) = GoatWidgetTest_Output {
      _goatWidgetTest_output_tool                  :: Dynamic t Tool
      , _goatWidgetTest_output_selection           :: Dynamic t Selection
      , _goatWidgetTest_output_layers              :: Dynamic t LayerEntries
      , _goatWidgetTest_output_pan                 :: Dynamic t XY
      , _goatWidgetTest_output_broadPhase          :: Dynamic t BroadPhaseState
      , _goatWidgetTest_output_handlerRenderOutput :: Dynamic t HandlerRenderOutput
      , _goatWidgetTest_output_canvas              :: Dynamic t SCanvas
      , _goatWidgetTest_output_DEBUG_goatState     :: Dynamic t GoatState
    }
  getApp GoatWidgetTest_InputEvents {..} = do
    let
      config = GoatWidgetConfig {
          _goatWidgetConfig_initialState    = _goatWidgetTest_inputEvents_initialState
          , _goatWidgetConfig_mouse         = _goatWidgetTest_inputEvents_mouse
          , _goatWidgetConfig_keyboard      = _goatWidgetTest_inputEvents_keyboard
          , _goatWidgetConfig_selectTool    = _goatWidgetTest_inputEvents_selectTool
          , _goatWidgetConfig_load          = _goatWidgetTest_inputEvents_load
          , _goatWidgetConfig_setDebugLabel = _goatWidgetTest_inputEvents_setDebugLabel
        }
    GoatWidget {..} <- holdGoatWidget config
    return $ GoatWidgetTest_Output {
        _goatWidgetTest_output_tool                  = _goatWidget_tool
        , _goatWidgetTest_output_selection           = _goatWidget_selection
        , _goatWidgetTest_output_layers              = _goatWidget_layers
        , _goatWidgetTest_output_pan                 = _goatWidget_pan
        , _goatWidgetTest_output_broadPhase          = _goatWidget_broadPhase
        , _goatWidgetTest_output_handlerRenderOutput = _goatWidget_handlerRenderOutput
        , _goatWidgetTest_output_canvas              = _goatWidget_canvas
        , _goatWidgetTest_output_DEBUG_goatState     = _goatWidget_DEBUG_goatState
      }
  makeInputs = do
    (inmouse, intrefmouse) <- newEventWithTriggerRef
    (inkb, intrefkb) <- newEventWithTriggerRef
    (intool, intreftool) <- newEventWithTriggerRef
    (inload, intrefload) <- newEventWithTriggerRef
    (indebug, intrefdebug) <- newEventWithTriggerRef
    return (
        GoatWidgetTest_InputEvents undefined inmouse inkb intool inload indebug
        , GoatWidgetTest_InputTriggerRefs intrefmouse intrefkb intreftool intrefload intrefdebug
      )

runGoatTestApp
  :: (TestGuestConstraints t m)
  => PFState
  -> ReflexTestT t (AppInputTriggerRefs (GoatWidgetTest t m)) (AppOutput (GoatWidgetTest t m)) m ()
  -> m ()
runGoatTestApp initialState rtm = do
  (inev, intref) <- makeInputs
  runReflexTestT (inev { _goatWidgetTest_inputEvents_initialState = initialState }, intref) getApp rtm
