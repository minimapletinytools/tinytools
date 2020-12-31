{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.BroadPhaseSpec(
  spec
) where

import           Relude       hiding (empty, fromList)

import           Test.Hspec

import           Data.Default (def)
import qualified Data.IntMap  as IM
import qualified Data.List    as L

import           Potato.Flow

spec :: Spec
spec = do
  describe "BroadPhase" $ do
    let
      makeChange rid lb = IM.singleton rid $ Just (SEltLabel (show rid) (SEltBox $ SBox lb def))
    it "passes basic test" $ do
      let
        lb1_2 = LBox (V2 0 0) (V2 5 5)
        lb1_1 = LBox (V2 0 0) (V2 10 10)
        lb2 = LBox (V2 5 5) (V2 5 5)
        lb3 = LBox (V2 0 5) (V2 5 5)
        lb4 = LBox (V2 5 0) (V2 5 5)
        bpt0 = BPTree $ IM.empty
        changes1 = makeChange 1 lb1_1
        BroadPhaseState aabbs1 bpt1 = update_bPTree changes1 bpt0
        culled1 = broadPhase_cull (L.head aabbs1) bpt1
      length aabbs1 `shouldBe` 1
      culled1 `shouldBe` [1]
      let
        changes2 = mconcat $ [makeChange 2 lb2, makeChange 3 lb3, makeChange 4 lb4]
        BroadPhaseState aabbs2 bpt2 = update_bPTree changes2 bpt1
        culled2 = broadPhase_cull lb1_1 bpt2
      length aabbs2 `shouldBe` 3
      length culled2 `shouldBe` 4
      let
        changes3 = makeChange 1 lb1_2
        BroadPhaseState aabbs3 bpt3 = update_bPTree changes3 bpt2
        culled3_1 = broadPhase_cull lb1_2 bpt3
        culled3_2 = broadPhase_cull (LBox (V2 3 3) (V2 5 5)) bpt3
      length aabbs3 `shouldBe` 2
      length culled3_1 `shouldBe` 1
      length culled3_2 `shouldBe` 4
    it "click on corner works as expected" $ do
      let
        box = LBox (V2 5 5) (V2 10 10)
        ul = LBox (V2 5 5) (V2 1 1)
        br = LBox (V2 14 14) (V2 1 1)
        notbr = LBox (V2 15 15) (V2 1 1)
        bpt0 = BPTree $ IM.empty
        changes = makeChange 1 box
        BroadPhaseState aabbs1 bpt1 = update_bPTree changes bpt0
        culled_ul = broadPhase_cull ul bpt1
        culled_br = broadPhase_cull br bpt1
        culled_notbr = broadPhase_cull notbr bpt1
      length culled_ul `shouldBe` 1
      length culled_br `shouldBe` 1
      length culled_notbr `shouldBe` 0
