{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.ManipulatorSpec
  ( spec
  )
where

import           Relude                        hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Reflex.Everything

import           Potato.Flow.Reflex.Common
import           Potato.Flow.TestStates

import           Data.Default
import qualified Data.IntMap                   as IM
import qualified Data.Sequence                 as Seq

basicStateWith4Boxes :: PFState
basicStateWith4Boxes = PFState {
      _pFState_layers = Seq.fromList [0..3]
      , _pFState_directory = IM.fromList [
          (0, SEltLabel "b1" (SEltBox SBox {
              _sBox_box = LBox (V2 0 0) (V2 5 5)
              , _sBox_style = def
            }))
          , (1, SEltLabel "b2" (SEltBox SBox {
              _sBox_box = LBox (V2 10 10) (V2 5 5)
              , _sBox_style = def
            }))
          , (2, SEltLabel "b3" (SEltBox SBox {
              _sBox_box = LBox (V2 0 10) (V2 5 5)
              , _sBox_style = def
            }))
          , (3, SEltLabel "b4" (SEltBox SBox {
              _sBox_box = LBox (V2 10 0) (V2 5 5)
              , _sBox_style = def
            }))
        ]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }

basic_sbox_test :: Test
basic_sbox_test = constructTest "manipulator - sbox" basicStateWith4Boxes bs expected where
  bs = [
      EWCLabel "select b2"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])
    ]
  expected = [
      LabelCheck "select b2"
      , (EqPredicate _everythingCombined_selectedTool Tool_Select)
      , AlwaysPass
      , numSelectedEltsEqualPredicate 1
    ]


spec :: Spec
spec = do
  describe "Manipulator" $ do
    fromHUnitTest $ basic_sbox_test
