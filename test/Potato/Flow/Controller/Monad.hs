-- monadic testing types for GoatWidget

{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Controller.Monad (
  checkSingle
  , checkSingleMaybe
  , GoatWidgetTest(..)
  , AppInputTriggerRefs(..)
  , AppInputEvents(..)
  , AppOutput(..)
  , runGoatTestApp

  -- you will always need methods from these modules in order to use this module
  , module Reflex.Test.Monad.Host
  , module Reflex.Host.Class
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

-- TODO check that there was only 1 output
checkSingle :: (HasCallStack, Eq a, Show a) => [a] -> a -> Assertion
checkSingle values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> a @=? head x

checkSingleMaybe :: (HasCallStack, Eq a, Show a) => [Maybe a] -> a -> Assertion
checkSingleMaybe values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> Just a @=? head x

data GoatWidgetTest t (m :: Type -> Type)

instance (TestGuestConstraints t m) => ReflexTestApp (GoatWidgetTest t m) t m where
  data AppInputTriggerRefs (GoatWidgetTest t m) = GoatWidgetTest_InputTriggerRefs {
      _goatWidgetTest_inputTriggerRefs_mouse         :: ReflexTriggerRef t m LMouseData
      , _goatWidgetTest_inputTriggerRefs_keyboard      :: ReflexTriggerRef t m KeyboardData
      , _goatWidgetTest_inputTriggerRefs_selectTool    :: ReflexTriggerRef t m Tool
      , _goatWidgetTest_inputTriggerRefs_load          :: ReflexTriggerRef t m EverythingLoadState
      , _goatWidgetTest_inputTriggerRefs_setDebugLabel :: ReflexTriggerRef t m Text
    }
  data AppInputEvents (GoatWidgetTest t m) = GoatWidgetTest_InputEvents (GoatWidgetConfig t)
  data AppOutput (GoatWidgetTest t m) = GoatWidgetTest_Output (GoatWidget t)
  getApp (GoatWidgetTest_InputEvents config) = do
    goatWidget <- holdGoatWidget config
    return $ GoatWidgetTest_Output goatWidget
  makeInputs = do
    (inmouse, intrefmouse) <- newEventWithTriggerRef
    (inkb, intrefkb) <- newEventWithTriggerRef
    (intool, intreftool) <- newEventWithTriggerRef
    (inload, intrefload) <- newEventWithTriggerRef
    (indebug, intrefdebug) <- newEventWithTriggerRef
    return (
        GoatWidgetTest_InputEvents (GoatWidgetConfig emptyPFState inmouse inkb intool inload indebug)
        , GoatWidgetTest_InputTriggerRefs intrefmouse intrefkb intreftool intrefload intrefdebug
      )

runGoatTestApp
  :: (TestGuestConstraints t m)
  => PFState
  -> ReflexTestT t (AppInputTriggerRefs (GoatWidgetTest t m)) (AppOutput (GoatWidgetTest t m)) m ()
  -> m ()
runGoatTestApp initialState rtm = do
  (GoatWidgetTest_InputEvents inev, intref) <- makeInputs
  let
    newConfig = GoatWidgetTest_InputEvents (inev { _goatWidgetConfig_initialState = initialState })
  runReflexTestT (newConfig, intref) getApp rtm
