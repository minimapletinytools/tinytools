-- monadic testing types for GoatWidget
-- NOTE, there's really no point to using this. Just make a State monad out of foldGoatFn instead
-- keeping this around as another example of how to use Reflex.Test.Monad.Host

{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Controller.Monad (
  checkSingle
  , checkSingleMaybe
  , checkSingleMaybePred
  , GoatWidgetTest(..)
  , AppInputTriggerRefs(..)
  , AppInputEvents(..)
  , AppOutput(..)
  , runGoatTestApp

  -- you will always need methods from these modules in order to use this module
  , module Reflex.Test.Monad.Host
  , module Reflex.Host.Class

  -- examples
  , spec
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
checkSingle values a = case values of
  []   -> assertFailure "empty list"
  x:[] -> a @=? x
  _    -> assertFailure "too many elts"

checkSingleMaybe :: (HasCallStack, Eq a, Show a) => [Maybe a] -> a -> Assertion
checkSingleMaybe values a = case values of
  []   -> assertFailure "empty list"
  x:[] -> Just a @=? x
  _    -> assertFailure "too many elts"

checkSingleMaybePred :: (HasCallStack, Show a) => [Maybe a] -> (a -> Bool) -> Assertion
checkSingleMaybePred values f = case values of
  []         -> assertFailure "empty list"
  Nothing:[] -> assertFailure "no value"
  Just x:[]  -> assertBool ("predicate failed: " <> show x) (f x)
  _          -> assertFailure "too many elts"

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
        GoatWidgetTest_InputEvents (GoatWidgetConfig emptyPFState Nothing inmouse inkb intool inload never never indebug)
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




everything_basic_monad_test :: Test
everything_basic_monad_test = TestLabel "basic monad" $ TestCase $ runSpiderHost $
  runGoatTestApp emptyPFState $ do

    -- get our app's input triggers
    GoatWidgetTest_InputTriggerRefs {..} <- inputTriggerRefs

    -- TODO move this into a helper
    -- get our app's output events and subscribe to them
    GoatWidgetTest_Output (GoatWidget {..}) <- outputs
    toolH <- subscribeEvent (updated _goatWidget_tool)

    -- TODO move this into a helper
    -- setup common ReadPhases
    let
      readToolEv = sequence =<< readEvent toolH

    -- fire an empty event
    fireQueuedEventsAndRead (return ())

    -- set the tool
    queueEventTriggerRef _goatWidgetTest_inputTriggerRefs_selectTool Tool_Pan
    fireQueuedEventsAndRead readToolEv >>= \a -> liftIO (checkSingleMaybe a Tool_Pan)


everything_saveload_monadtest :: Test
everything_saveload_monadtest = TestLabel "saveload" $ TestCase $ runSpiderHost $
  runGoatTestApp emptyPFState $ do

    -- get our app's input triggers
    GoatWidgetTest_InputTriggerRefs {..} <- inputTriggerRefs

    -- TODO move this into a helper
    -- get our app's output events and subscribe to them
    GoatWidgetTest_Output (GoatWidget {..}) <- outputs
    goatStateH <- subscribeEvent (updated _goatWidget_DEBUG_goatState)

    -- TODO move this into a helper
    -- setup common ReadPhases
    let
      --readGoatState = sample . current $ _goatWidget_DEBUG_goatState
      readGoatStateEv = sequence =<< readEvent goatStateH

    -- load and check the state is the same
    queueEventTriggerRef _goatWidgetTest_inputTriggerRefs_load $ (pFState_to_sPotatoFlow pfstate_basic1, emptyControllerMeta)
    fireQueuedEventsAndRead readGoatStateEv >>= \a -> liftIO (checkSingleMaybePred a $ \gs ->
      _pFWorkspace_pFState (_goatState_pFWorkspace gs) == pfstate_basic1)

spec :: Spec
spec = do
  describe "Monad" $ do
    fromHUnitTest $ everything_saveload_monadtest
    fromHUnitTest $ everything_basic_monad_test
