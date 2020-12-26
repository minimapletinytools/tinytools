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




-- this should work with any initial state so long as default names aren't used
test_LayersHandler_basic :: Test
test_LayersHandler_basic = constructTest "basic" pfstate_basic1 bs expected where
  bs = [

      EWCLabel "select"
      , EWCMouse (LMouseData (V2 5 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 5 0) True MouseButton_Left [] True)

      , EWCLabel "deselect"
      , EWCMouse (LMouseData (V2 5 0) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 5 0) True MouseButton_Left [KeyModifier_Shift] True)

      , EWCLabel "select and cancel"
      , EWCMouse (LMouseData (V2 5 0) False MouseButton_Left [] True)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 5 0) True MouseButton_Left [] True)

      , EWCLabel "shift select 2 elts"
      , EWCMouse (LMouseData (V2 5 0) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 5 0) True MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 5 1) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 5 1) True MouseButton_Left [KeyModifier_Shift] True)
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

spec :: Spec
spec = do
  describe "LayersHandler" $ do
    fromHUnitTest $ test_LayersHandler_basic
