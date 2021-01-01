{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.BoxTextSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.BoxText

import           Data.Default
import qualified Data.Text.Zipper                           as TZ

testText1 :: Text
testText1 = "aoeu\nhi\n12345wrapping"

testSText1 :: SBox
testSText1 = def {
    _sBox_box = LBox (V2 5 5) (V2 5 10)
    , _sBox_text = Just $ SBoxText testText1 def
  }

testClick :: Int -> Int -> RelMouseDrag
testClick x y = RelMouseDrag $ def {
    _mouseDrag_to = V2 x y
  }

makeBoxTextInputState_basic_test :: Spec
makeBoxTextInputState_basic_test = let
    tais1 = makeBoxTextInputState testSText1 (testClick 5 5)
    tais2 = mouseText (Just tais1) testSText1 (testClick 6 5)
  in
    it "makeBoxTextInputState_basic" $ do
      --traceShow tais1 $ traceShow tais2 $ 1 `shouldBe` 1
      _boxTextInputState_original tais1 `shouldBe` testText1
      _boxTextInputState_original tais2 `shouldBe` testText1
      -- TZ has no Eq instance but show works fine, whatever
      show (TZ.right (_boxTextInputState_zipper tais1)) `shouldBe` show (_boxTextInputState_zipper tais2)

-- this causes infinite loop :(
--instance Eq TZ.TextZipper

spec :: Spec
spec = do
  describe "BoxText" $ do
    makeBoxTextInputState_basic_test
