{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.BroadPhaseSpec(
  spec
) where

import           Relude       hiding (empty, fromList)

import           Test.Hspec

import           Data.Default (def)
import qualified Data.IntMap  as IM
import qualified Data.List    as L
import qualified Data.Text    as T

import           Potato.Flow

spec :: Spec
spec = do
  describe "BroadPhase" $ do
    it "passes basic test" $ do
      let
        lb1_2 = LBox (V2 0 0) (V2 5 5)
        lb1_1 = LBox (V2 0 0) (V2 10 10)
        lb2 = LBox (V2 0 0) (V2 5 5)
        lb3 = LBox (V2 0 5) (V2 5 5)
        lb4 = LBox (V2 5 0) (V2 5 5)
        makeChange rid lb = IM.singleton rid $ Just (SEltLabel (show rid) (SEltBox $ SBox lb def))

        bpt0 = BPTree $ IM.empty
        changes1 = makeChange 1 lb1_1
        (aabbs1, bpt1, _) = update_bPTree changes1 bpt0
        culled1 = broadPhase_cull (L.head aabbs1) bpt1
      length aabbs1 `shouldBe` 1
      culled1 `shouldBe` [1]
      let
        changes2 = mconcat $ [makeChange 2 lb2, makeChange 3 lb3, makeChange 4 lb4]
        (aabbs2, bpt2, _) = update_bPTree changes2 bpt1
        culled2 = broadPhase_cull lb1_1 bpt2
      length aabbs2 `shouldBe` 3
      length culled2 `shouldBe` 4
      let
        changes3 = makeChange 1 lb1_2
        (aabbs3, bpt3, _) = update_bPTree changes3 bpt2
        -- note we cull the new box, which is not what we would normally do in rendering
        culled3 = broadPhase_cull lb1_2 bpt3
      length aabbs3 `shouldBe` 2
      length culled3 `shouldBe` 2
