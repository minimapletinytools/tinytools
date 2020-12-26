{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LayersSpec
  ( spec
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.GoatWidget
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input

import           Potato.Flow.Common
import           Potato.Flow.TestStates

import           Data.Default
import qualified Data.IntMap                       as IM
import qualified Data.Sequence                     as Seq

moveOffset :: Int
moveOffset = 5

collapseOffset :: Int
collapseOffset = 0

hideOffset :: Int
hideOffset = 1

lockOffset :: Int
lockOffset = 2



numVisibleHiddenEltsEqualPredicate :: Int -> EverythingPredicate
numVisibleHiddenEltsEqualPredicate n = FunctionPredicate $
  (\(_, lentries) ->
    let nhidden = length $ filter (lockHiddenStateToBool . _layerEntry_hideState) (toList lentries)
    in ("Hidden: " <> show nhidden <> " expected: " <> show n <> " lentries: " <> layerEntriesToPrettyText lentries, nhidden == n))
  . _goatState_layersState

numVisibleLockedEltsEqualPredicate :: Int -> EverythingPredicate
numVisibleLockedEltsEqualPredicate n = FunctionPredicate $
  (\(_, lentries) ->
    let nlocked = length $ filter (lockHiddenStateToBool . _layerEntry_lockState) (toList lentries)
    in ("Locked: " <> show nlocked <> " expected: " <> show n <> " lentries:\n" <> layerEntriesToPrettyText lentries, nlocked == n))
  . _goatState_layersState

-- this should work with any initial state so long as default names aren't used
test_LayersHandler_basic :: Test
test_LayersHandler_basic = constructTest "basic" pfstate_basic1 bs expected where
  bs = [

      EWCLabel "select"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "deselect"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [KeyModifier_Shift] True)

      , EWCLabel "select and cancel"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "shift select 2 elts"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) True MouseButton_Left [KeyModifier_Shift] True)
    ]
  expected = [
      LabelCheck "select"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "deselect"
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "select and cancel"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "shift select 2 elts"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 2
    ]

test_LayersHandler_toggle :: Test
test_LayersHandler_toggle = constructTest "toggle" pfstate_basic1 bs expected where
  bs = [
      EWCLabel "lock"
      , EWCNothing
      , EWCMouse (LMouseData (V2 lockOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 lockOffset 0) True MouseButton_Left [] True)

      , EWCLabel "hide"
      , EWCNothing
      , EWCMouse (LMouseData (V2 hideOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 hideOffset 0) True MouseButton_Left [] True)

      -- TODO inherit/folder test stuff
    ]
  expected = [
      LabelCheck "lock"
      , numVisibleLockedEltsEqualPredicate 0
      , numVisibleLockedEltsEqualPredicate 1
      , numVisibleLockedEltsEqualPredicate 1

      , LabelCheck "hide"
      , numVisibleHiddenEltsEqualPredicate 0
      , numVisibleHiddenEltsEqualPredicate 1
      , numVisibleHiddenEltsEqualPredicate 1
    ]

test_LayersHandler_move :: Test
test_LayersHandler_move = constructTest "move" pfstate_basic1 bs expected where
  bs = [

      EWCLabel "select b1"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "drag b1"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 4) True MouseButton_Left [] True)

    ]
  expected = [
      LabelCheck "select b1"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "drag b1"
      , AlwaysPass
      , firstSelectedSuperSEltLabelPredicate (Just "b1") $
        \(_,lp,_) -> lp == 3
    ]

spec :: Spec
spec = do
  describe "LayersHandler" $ do
    fromHUnitTest $ test_LayersHandler_basic
    fromHUnitTest $ test_LayersHandler_toggle
    fromHUnitTest $ test_LayersHandler_move
