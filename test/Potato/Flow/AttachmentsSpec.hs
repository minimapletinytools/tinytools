{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.AttachmentsSpec (
  spec
) where

import           Relude           hiding (empty, fromList)

import           Test.Hspec

import           Potato.Flow.Attachments
import           Potato.Flow.Math
import Potato.Flow.SElts

spec :: Spec
spec = do
  describe "Attachments" $ do
    it "attachLocationFromLBox" $ do
      let
        somelbox1 = LBox (V2 0 0) (V2 1 1)
        somelbox2 = LBox (V2 0 0) (V2 9 10)
      attachLocationFromLBox False somelbox1 AL_TOP `shouldBe` V2 0 0
      attachLocationFromLBox False somelbox1 AL_LEFT `shouldBe` V2 0 0
      attachLocationFromLBox True somelbox2 AL_BOT `shouldBe` V2 4 10
      attachLocationFromLBox True somelbox2 AL_RIGHT `shouldBe` V2 9 5