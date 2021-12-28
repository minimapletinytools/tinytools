{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Flow.Deprecated.LayersSpec(
  spec
) where

import           Relude

import           Test.Hspec

import qualified Data.Sequence            as Seq

import           Potato.Flow.Deprecated.Layers

someSeq1 :: Seq.Seq Int
someSeq1 = Seq.fromList [0,0,0,0,0]

someSeq2 :: Seq.Seq Int
someSeq2 = Seq.fromList [1,2,3]

someSeq3 :: Seq.Seq Int
someSeq3 = Seq.fromList [0..9]

scopeFn :: Int -> Maybe Bool
scopeFn 0 = Nothing
scopeFn 1 = Just True
scopeFn 2 = Just False

scopingSeq1 :: Seq.Seq Int
scopingSeq1 = Seq.fromList [0,0,0,0,1,1,1,2,2,1,2,1,2,1,2,1,1,1,2,2,2,2]
scopingSeq2 :: Seq.Seq Int
scopingSeq2 = Seq.fromList [0,0,0,0,1,1,1,2,0,1,2,1,2,1,2,1,1,1,2,2,2,2]

spec :: Spec
spec = do
  describe "Layers" $ do
    it "reindexing" $ do
      reindexSEltLayerPosForRemoval [0..9] `shouldBe` [0 | _ <- [0..9]]
      reindexSEltLayerPosForInsertion [0..9] `shouldBe` [0..9]
    it "hasScopingProperty" $ do
      hasScopingProperty scopeFn scopingSeq1 `shouldBe` True
      hasScopingProperty scopeFn scopingSeq2 `shouldBe` False
    it "selectionHasScopingProperty" $ do
      selectionHasScopingProperty scopeFn scopingSeq1 [4,5,20,21] `shouldBe` True
      selectionHasScopingProperty scopeFn scopingSeq1 [0..21] `shouldBe` True
      selectionHasScopingProperty scopeFn scopingSeq1 [0..20] `shouldBe` False
      selectionHasScopingProperty scopeFn scopingSeq2 ([0..4]<>[6..21]) `shouldBe` True
    it "findMatchingScope" $ do
      findMatchingScope scopeFn scopingSeq1 4 `shouldBe` 21
    it "scopeSelection" $ do
      scopeSelection scopeFn scopingSeq1 [0,4,5,17] `shouldBe` [0,4,5,8,17,18,21]
      scopeSelection scopeFn scopingSeq1 [0..3] `shouldBe` [0..3]
      scopeSelection scopeFn scopingSeq1 [18..21] `shouldBe` [4]<>[15..21]
    it "insertElts" $ do
      insertElts 2 someSeq2 someSeq1 `shouldBe` Seq.fromList [0,0,1,2,3,0,0,0]
    it "insertElt" $ do
      insertElt 5 1 someSeq1 `shouldBe` Seq.fromList [0,0,0,0,0,1]
    it "removeElts" $ do
      removeElts 2 0 someSeq2 `shouldBe` Seq.fromList [3]
    it "insertEltList_indexBeforeInsertion" $ do
      insertEltList_indexBeforeInsertion [(0,1),(2,1),(4,1)] someSeq1 `shouldBe` Seq.fromList [1,0,0,1,0,0,1,0]
    it "insertEltList_indexAfterInsertion" $ do
      insertEltList_indexAfterInsertion [(0,1),(2,1),(4,1)] someSeq1 `shouldBe` Seq.fromList [1,0,1,0,1,0,0,0]
    it "removeEltList" $ do
      removeEltList [0..5] someSeq3 `shouldBe` Seq.fromList [6..9]
      removeEltList [0,1,5,9] someSeq3 `shouldBe` Seq.fromList [2,3,4,6,7,8]
    it "moveEltList" $ do
      moveEltList [0,1,5,9] 8 someSeq3 `shouldBe` Seq.fromList [2,3,4,6,7,0,1,5,9,8]
    it "undoMoveEltList" $ do
      undoMoveEltList [0,1,5,9] 8 (Seq.fromList [2,3,4,6,7,0,1,5,9,8]) `shouldBe` someSeq3
