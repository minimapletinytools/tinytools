module Potato.Flow.OwlSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Sequence            as Seq
import           Potato.Flow.Owl
import           Potato.Flow.TestStates
import           Potato.Flow.State

spec :: Spec
spec = do
  describe "Owl" $ do
    it "dummy" $ do
      2+2 `shouldBe` 4
    it "basic" $ do
      let
        od = owlDirectory_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      Seq.length (owlDirectory_topSuperOwls od) `shouldBe` 1
