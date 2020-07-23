{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Flow.New.StateSpec(
  spec
) where

import           Relude

import           Test.Hspec

import qualified Data.Sequence            as Seq
import           Potato.Flow

import           Potato.Flow.New.State
import           Potato.Flow.Reflex.Types

spec :: Spec
spec = do
  describe "Layers" $ do
    it "SPotatoFlow <-> PFState conversion passes basic tests" $ do
      let
        orig = SPotatoFlow (SCanvas (LBox 10 123)) [SEltLabel "some selt" SEltNone]
      pFState_to_sPotatoFlow (sPotatoFlow_to_pFState orig) `shouldBe` orig
