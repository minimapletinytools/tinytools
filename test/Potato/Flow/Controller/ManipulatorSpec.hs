{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.ManipulatorSpec
  ( spec
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.Everything
import           Potato.Flow.Controller.Input

import           Potato.Flow.Common
import           Potato.Flow.TestStates

import           Data.Default
import qualified Data.IntMap                       as IM
import qualified Data.Sequence                     as Seq

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
      EWCTool Tool_Select

      , EWCLabel "select b2"
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])

      , EWCLabel "resize tl corner b2"
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])

      , EWCLabel "resize tr corner b2"
      , EWCMouse (LMouseData (V2 15 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 15 8) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 15 8) True MouseButton_Left [])

      , EWCLabel "resize bl corner b2"
      , EWCMouse (LMouseData (V2 10 15) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 7 15) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 7 15) True MouseButton_Left [])

      , EWCLabel "resize br corner b2"
      , EWCMouse (LMouseData (V2 15 15) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [])

      , EWCLabel "area move b2"
      , EWCMouse (LMouseData (V2 15 15) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [])
    ]
  expected = [
      EqPredicate _everythingCombined_selectedTool Tool_Select

      , LabelCheck "select b2"
      , AlwaysPass
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "resize tl corner b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperSEltLabelPredicate (Just "b2") $ \(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox lbox _) -> lbox == LBox (V2 11 11) (V2 4 4)
        _                     -> False

      , LabelCheck "resize tr corner b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperSEltLabelPredicate (Just "b2") $ \(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox lbox _) -> lbox == LBox (V2 11 9) (V2 4 6)
        _                     -> False

      , LabelCheck "resize bl corner b2"
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass

      , LabelCheck "resize br corner b2"
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass

      , LabelCheck "area move b2"
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass
    ]

restrict8_test :: Test
restrict8_test = constructTest "manipulator - restrict8" basicStateWith4Boxes bs expected where
  bs = [
      EWCTool Tool_Select

      , EWCLabel "select b2"
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])

      , EWCLabel "resize tl corner b2 while holding shift"
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [KeyModifier_Shift])
      , EWCMouse (LMouseData (V2 10 0) False MouseButton_Left [KeyModifier_Shift])
      , EWCMouse (LMouseData (V2 10 0) True MouseButton_Left [KeyModifier_Shift])
    ]
  expected = [
      EqPredicate _everythingCombined_selectedTool Tool_Select

      , LabelCheck "select b2"
      , AlwaysPass
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "resize tl corner b2 while holding shift"
      , AlwaysPass
      , AlwaysPass
      , firstSuperSEltLabelPredicate (Just "b2") $ \(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 10 && y == 1
        _                                  -> False
    ]


spec :: Spec
spec = do
  describe "Manipulator" $ do
    fromHUnitTest $ basic_sbox_test
    fromHUnitTest $ restrict8_test
