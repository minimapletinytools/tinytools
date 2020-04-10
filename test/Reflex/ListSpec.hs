module Reflex.ListSpec (
  spec
) where

import           Relude

import           Test.Hspec

test_spec :: Spec
test_spec = describe "test hspec" $ do
  it "works" $
    True `shouldBe` True


spec :: Spec
spec = do
  describe "hspec" $ do
    test_spec
