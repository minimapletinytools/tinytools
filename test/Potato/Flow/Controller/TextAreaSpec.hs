{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.TextAreaSpec
  ( spec
  )
where

import           Relude                          hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit        (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.TextArea

import           Data.Default
import qualified Data.Text.Zipper                as TZ

testText1 :: Text
testText1 = "aoeu\nhi\n12345wrapping"

testSText1 :: SText
testSText1 = SText {
    _sText_box = LBox (V2 5 5) (V2 5 10)
    , _sText_text = testText1
    , _sText_style = def
  }

testClick :: Int -> Int -> RelMouseDrag
testClick x y = RelMouseDrag $ emptyMouseDrag {
    _mouseDrag_to = V2 x y
  }

makeTextAreaInputState_basic_test :: Spec
makeTextAreaInputState_basic_test = let
    tais1 = makeTextAreaInputState testSText1 (testClick 5 5)
    tais2 = mouseText (Just tais1) testSText1 (testClick 6 5)
  in
    it "makeTextAreaInputState_basic" $ do
      --traceShow tais1 $ traceShow tais2 $ 1 `shouldBe` 1
      _textAreaInputState_original tais1 `shouldBe` testText1
      _textAreaInputState_selected tais1 `shouldBe` 0
      _textAreaInputState_original tais2 `shouldBe` testText1
      -- whatever
      show (TZ.right (_textAreaInputState_zipper tais1)) `shouldBe` show (_textAreaInputState_zipper tais2)

-- this causes infinite loop :(
--instance Eq TZ.TextZipper

spec :: Spec
spec = do
  describe "TextArea" $ do
    makeTextAreaInputState_basic_test
