{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.BoxSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow

import           Potato.Flow.Common

import           Data.Default
import           Data.Dependent.Sum                         (DSum ((:=>)))
import qualified Data.IntMap                                as IM
import qualified Potato.Data.Text.Zipper                           as TZ




test_basic :: Test
test_basic = constructTest "keyboard movement" emptyOwlPFState bs expected where
  bs = [
      EWCLabel "Create A"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "KB move A"
      , EWCKeyboard (KeyboardData (KeyboardKey_Up) [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Down) [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Left) [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Right) [])

    ]
  testFirstBoxIs lbox = firstSuperOwlPredicate (Just "<box>") $ \sowl -> case isOwl_toSElt_hack sowl of
    SEltBox (SBox lbox _ _ _ _) -> lbox == lbox
    _                           -> False
  expected = [
      LabelCheck "Create A"
      , EqPredicate _goatState_selectedTool Tool_Box
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          testFirstBoxIs (LBox (V2 1 1) (V2 9 9))
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_box False
        ]

      , LabelCheck "KB move A"
      , testFirstBoxIs (LBox (V2 1 0) (V2 9 9))
      , testFirstBoxIs (LBox (V2 1 1) (V2 9 9))
      , testFirstBoxIs (LBox (V2 0 1) (V2 9 9))
      , testFirstBoxIs (LBox (V2 1 1) (V2 9 9))

    ]

spec :: Spec
spec = do
  describe "Box" $ do
    fromHUnitTest $ test_basic
