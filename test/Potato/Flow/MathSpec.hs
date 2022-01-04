{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.MathSpec(
  spec
) where

import           Relude           hiding (empty, fromList)

import           Test.Hspec

import           Data.Default     (def)

import           Potato.Flow.Math


spec :: Spec
spec = do
  describe "Math" $ do
    it "add_XY_to_lBox" $ do
      add_XY_to_lBox 0 (LBox 1 1) `shouldBe` LBox 0 2
      add_XY_to_lBox 3 (LBox 1 1) `shouldBe` LBox 1 2
    it "lBox_expand" $ do
      lBox_expand (LBox (V2 0 0) (V2 10 10)) (1,1,1,1) `shouldBe` (LBox (V2 (-1) (-1)) (V2 12 12))
    it "substract_lBox" $ do
      let
        lb1 = LBox (V2 0 0) (V2 10 10)
        lb2 = LBox (V2 5 5) (V2 10 10)
        lb3 = LBox (V2 2 2) (V2 6 6)
      length (substract_lBox lb1 lb2) `shouldBe` 2
      length (substract_lBox lb2 lb1) `shouldBe` 2
      length (substract_lBox lb1 lb3) `shouldBe` 4
      length (substract_lBox lb3 lb1) `shouldBe` 0
