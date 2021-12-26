{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LinesSpec(
  spec
) where

import           Relude           hiding (empty, fromList)

import           Test.Hspec

import           Data.Default     (def)

import           Potato.Flow.Methods.Lines
import           Potato.Flow.Math


spec :: Spec
spec = do
  describe "Lines" $ do
    it "determineSeparation" $ do
      let
        lb1 = LBox (V2 0 0) (V2 10 10)
        lb2 = LBox (V2 11 11) (V2 10 10)
      determineSeparation (lb1, (0,0,0,0)) (lb2, (0,0,0,0)) `shouldBe` (True, True)
      determineSeparation (lb1, (2,2,0,0)) (lb2, (0,0,0,0)) `shouldBe` (False, True)
      determineSeparation (lb1, (1,1,1,1)) (lb2, (1,1,1,1)) `shouldBe` (False, False)
