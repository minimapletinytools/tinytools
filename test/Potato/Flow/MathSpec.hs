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
    it "substract_lBox" $ do
      let
        lb1 = LBox (V2 0 0) (V2 10 10)
        lb2 = LBox (V2 5 5) (V2 10 10)
        lb3 = LBox (V2 2 2) (V2 6 6)
      length (substract_lBox lb1 lb2) `shouldBe` 2
      length (substract_lBox lb2 lb1) `shouldBe` 2
      length (substract_lBox lb1 lb3) `shouldBe` 4
      length (substract_lBox lb3 lb1) `shouldBe` 0
