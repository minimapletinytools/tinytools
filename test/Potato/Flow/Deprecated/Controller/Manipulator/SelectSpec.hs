{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Deprecated.Controller.Manipulator.SelectSpec
  ( spec
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Common
import           Potato.Flow.TestStates


-- attempt to select 0 area objects and ensure behavior is correct
test_Select_zero :: Test
test_Select_zero = constructTest "zero" owlpfstate_zero bs expected where
  bs = [

      EWCLabel "select b1"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] False)

      , EWCLabel "deselect"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "select sl1"
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 11) True MouseButton_Left [] False)
    ]
  expected = [
      LabelCheck "select b1"
      , EqPredicate goatState_selectedTool Tool_Select
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "deselect"
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "select sl1"
      , numSelectedEltsEqualPredicate 0
      -- not possible to select zero area lines ATM (TODO fix)
      , numSelectedEltsEqualPredicate 0
    ]


spec :: Spec
spec = do
  describe "SelectHandler" $ do
    fromHUnitTest $ test_Select_zero
