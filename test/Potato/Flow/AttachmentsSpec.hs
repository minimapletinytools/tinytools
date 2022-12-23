{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.AttachmentsSpec (
  spec
) where

import           Relude           hiding (empty, fromList)

import           Test.Hspec

import           Potato.Flow.Attachments
import Potato.Flow.Methods.LineDrawer
import Potato.Flow.Methods.LineTypes
import           Potato.Flow.Math
import Potato.Flow.SElts

spec :: Spec
spec = do
  describe "Attachments" $ do
    it "attachLocationFromLBox" $ do
      let
        somelbox1 = LBox (V2 0 0) (V2 1 1)
        somelbox2 = LBox (V2 0 0) (V2 9 10)
      attachLocationFromLBox False somelbox1 AL_Top `shouldBe` V2 0 0
      attachLocationFromLBox False somelbox1 AL_Left `shouldBe` V2 0 0
      attachLocationFromLBox True somelbox2 AL_Bot `shouldBe` V2 4 10
      attachLocationFromLBox True somelbox2 AL_Right `shouldBe` V2 9 5

{- TODO fix me later.... or delete!
    it "attachLocationFromLBox_conjugation_invariance" $ do
      let
        somelboxes = [
            LBox (V2 0 0) (V2 0 0)
            , LBox (V2 0 0) (V2 1 1)
            , LBox (V2 0 0) (V2 2 2)
            , LBox (V2 0 0) (V2 20 25)
            , LBox (V2 0 0) (V2 25 20)
          ]
        conjugations = [
            (transformMe_rotateLeft, transformMe_rotateRight)
            , (transformMe_rotateRight . transformMe_rotateRight, transformMe_rotateLeft . transformMe_rotateLeft)
          ]
      forM_ somelboxes $ \lbox ->
        forM_ conjugations $ \(c, cc) ->
          forM_ [True, False] $ \offsetBorder -> 
            forM_ [AL_Top, AL_Left, AL_Bot, AL_Right] $ \al -> do
              attachLocationFromLBox offsetBorder lbox al `shouldBe` cc (attachLocationFromLBox offsetBorder (c lbox) al)
              --attachLocationFromLBox offsetBorder lbox al `shouldBe` c (attachLocationFromLBox offsetBorder (cc lbox) al)
-}    





