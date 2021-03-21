module Potato.Flow.OwlSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Sequence            as Seq
import           Potato.Flow.Owl

spec :: Spec
spec = do
  describe "Owl" $ do
    it "dummy" $ do
      2+2 `shouldBe` 4
