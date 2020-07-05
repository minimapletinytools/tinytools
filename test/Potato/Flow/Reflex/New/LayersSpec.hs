{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Flow.Reflex.New.LayersSpec(
  spec
) where

import           Relude

import           Test.Hspec

import qualified Data.Sequence                 as Seq
import           Potato.Flow

import           Potato.Flow.Reflex.New.Layers
import           Potato.Flow.Reflex.Types

someSeq1 :: Seq.Seq Int
someSeq1 = Seq.fromList [0,0,0,0,0]

someSeq2 :: Seq.Seq Int
someSeq2 = Seq.fromList [1,2,3]

someSeq3 :: Seq.Seq Int
someSeq3 = Seq.fromList [0..9]

spec :: Spec
spec = do
  describe "Layers" $ do
    it "reindexing" $ do
      reindexSEltLayerPosForRemoval [0..9] `shouldBe` [0 | x <- [0..9]]
      reindexSEltLayerPosForInsertion [0..9] `shouldBe` [0..9]
    it "insertElts" $ do
      insertElts 2 someSeq2 someSeq1 `shouldBe` Seq.fromList [0,0,1,2,3,0,0,0]
    it "insertElt" $ do
      insertElt 5 1 someSeq1 `shouldBe` Seq.fromList [0,0,0,0,0,1]
    it "removeElts" $ do
      removeElts 2 0 someSeq2 `shouldBe` Seq.fromList [3]
    it "insertEltList" $ do
      insertEltList [(0,1),(2,1),(4,1)] someSeq1 `shouldBe` Seq.fromList [1,0,0,1,0,0,1,0]
    it "removeEltList" $ do
      removeEltList [0..5] someSeq3 `shouldBe` Seq.fromList [6..9]
