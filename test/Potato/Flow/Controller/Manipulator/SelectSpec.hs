{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.SelectSpec
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
test_Select_zero :: Test
test_Select_zero = constructTest "zero" pfstate_zero bs expected where
  bs = [

      EWCLabel "select b1"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] False)

      , EWCLabel "deselect"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      -- sl1 isn't actually zero area because SSimpleLine can't be zero area
      , EWCLabel "select sl1"
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 11) True MouseButton_Left [] False)
    ]
  expected = [
      LabelCheck "select b1"
      , EqPredicate _goatState_selectedTool Tool_Select
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "deselect"
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "select sl1"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1
    ]


spec :: Spec
spec = do
  describe "SelectHandler" $ do
    fromHUnitTest $ test_Select_zero
