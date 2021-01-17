{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Data.Text.ZipperSpec(
  spec
) where

import           Relude

import           Test.Hspec

import qualified Data.IntMap            as IM
import qualified Data.Sequence          as Seq

import Potato.Data.Text.Zipper


someSentence = "12345 1234 12"

splitSentenceAtDisplayWidth :: Int -> Text -> [(Text, Bool)]
splitSentenceAtDisplayWidth w t = splitWordsAtDisplayWidth w (wordsWithWhitespace t)

spec :: Spec
spec = do
  describe "Zipper" $ do
    it "wordsWithWhitespace" $ do
      wordsWithWhitespace "" `shouldBe` []
      wordsWithWhitespace "😱😱😱" `shouldBe` ["😱😱😱"]
      wordsWithWhitespace "abcd efgf f" `shouldBe` ["abcd ","efgf ", "f"]
      wordsWithWhitespace "aoeu    " `shouldBe` ["aoeu    "]
    it "splitWordsAtDisplayWidth" $ do
      fmap fst (splitSentenceAtDisplayWidth 5 "123456") `shouldBe` ["12345","6"]
      fmap fst (splitSentenceAtDisplayWidth 5 "12345 6") `shouldBe` ["12345","6"]
      fmap fst (splitSentenceAtDisplayWidth 5 "1234 56") `shouldBe` ["1234 ","56"]
      fmap fst (splitSentenceAtDisplayWidth 5 "12345678912345") `shouldBe` ["12345","67891","2345"]
      fmap fst (splitSentenceAtDisplayWidth 5 "1234   56") `shouldBe` ["1234 "," 56"]
      fmap fst (splitSentenceAtDisplayWidth 8 "1 2 3 4 5 6 7 8 9 1") `shouldBe` ["1 2 3 4 ","5 6 7 8 ", "9 1"]
    it "wrapWithOffsetAndAlignment" $ do
      wrapWithOffsetAndAlignment TextAlignment_Left 5 0 someSentence `shouldBe` [("12345", True, 0), ("1234 ", False, 0), ("12", False, 0)]
      wrapWithOffsetAndAlignment TextAlignment_Right 5 0 someSentence `shouldBe` [("12345", True, 0), ("1234 ", False, 0), ("12", False, 3)]
      wrapWithOffsetAndAlignment TextAlignment_Center 5 0 someSentence `shouldBe` [("12345", True, 0), ("1234 ", False, 0), ("12", False, 1)]
