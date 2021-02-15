{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Data.Text.ZipperSpec(
  spec
) where

import           Relude

import           Test.Hspec

import qualified Data.Map            as Map

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
      fmap fst (splitSentenceAtDisplayWidth 5 "1234 56") `shouldBe` ["1234","56"]
      fmap fst (splitSentenceAtDisplayWidth 5 "12345678912345") `shouldBe` ["12345","67891","2345"]
      fmap fst (splitSentenceAtDisplayWidth 5 "1234   56") `shouldBe` ["1234 "," 56"]
      fmap fst (splitSentenceAtDisplayWidth 8 "1 2 3 4 5 6 7 8 9 1") `shouldBe` ["1 2 3 4","5 6 7 8", "9 1"]
    it "wrapWithOffsetAndAlignment" $ do
      wrapWithOffsetAndAlignment TextAlignment_Left 5 0 someSentence `shouldBe` [("12345", True, 0), ("1234", True, 0), ("12", False, 0)]
      wrapWithOffsetAndAlignment TextAlignment_Right 5 0 someSentence `shouldBe` [("12345", True, 0), ("1234", True, 1), ("12", False, 3)]
      wrapWithOffsetAndAlignment TextAlignment_Center 5 0 someSentence `shouldBe` [("12345", True, 0), ("1234", True, 0), ("12", False, 1)]
    it "eolSpacesToLogicalLines" $ do
      eolSpacesToLogicalLines
        [
          [ ("😱",True,1), ("😱",False,2), ("😱",False,3) ]
          , [ ("aa",True,1), ("aa",True,2), ("aa",False,3) ]
        ]
        `shouldBe`
        [
          [ ("😱",1) ]
          , [ ("😱",2), ("😱",3) ]
          , [ ("aa",1) ]
          , [ ("aa",2) ]
          , [ ("aa",3) ]
        ]
    it "offsetMapWithAlignmentInternal" $ do
      offsetMapWithAlignmentInternal
        [
          [ ("😱",True,1), ("😱",False,2), ("😱",False,3) ]
          , [ ("aa",True,1), ("aa",True,2), ("aa",False,3) ]
        ]
        `shouldBe`
        Map.fromList
        [ (0, (1,0))
        , (1, (2,2)) -- jump by 1 for char and 1 for space
        , (2, (3,3)) -- jump by 1 for char
        , (3, (1,5)) -- jump by 1 for char and 1 for newline
        , (4, (2,8)) -- jump by 2 for char and 1 for space
        , (5, (3,11)) -- jump by 2 for char and 1 for space
        ]
    --it "displayLinesWithAlignment - empty" $ do
      --let
      --  dl = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "")
      -- fails, produces [Span () "", Span () " "] because it creates an space character to for the cursor
      --_displayLinesWithAlignment_spans dl `shouldBe` [Span () "", Span () ""]
    it "displayLinesWithAlignment - cursorPos" $ do
      let
        dl0 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "")
        dl1 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu")
        dl2 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu\n")
        dl3 = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "0123456789")
      _displayLinesWithAlignment_cursorPos dl0 `shouldBe` (0,0)
      _displayLinesWithAlignment_cursorPos dl1 `shouldBe` (4,0)
      _displayLinesWithAlignment_cursorPos dl2 `shouldBe` (0,1)
      _displayLinesWithAlignment_cursorPos dl3 `shouldBe` (0,1)
    --it "displayLinesWithAlignment" $ do
      --let
        --dl = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "aoeu ansthe\naoeuo\naone , aoentuh ao anhno ao ao ou unh sarc.,as eohaun oeuoeu nthoeu noeuhnooeu")
        --dl = displayLinesWithAlignment TextAlignment_Right 10 () () (fromText "")
      --traceShow (_displayLinesWithAlignment_spans dl) (True `shouldBe` True)
