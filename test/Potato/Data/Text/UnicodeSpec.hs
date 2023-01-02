{-# LANGUAGE OverloadedStrings #-}

module Potato.Data.Text.UnicodeSpec (
  spec
) where

import           Prelude

import           Test.Hspec

import           Control.Monad

import           Potato.Data.Text.Unicode



spec :: Spec
spec = describe "Unicode" $ do
  it "isSingleGraphemeCluster" $ do
    isSingleGraphemeCluster "👎🏼" `shouldBe` True
    isSingleGraphemeCluster "👎" `shouldBe` False
    isSingleGraphemeCluster "" `shouldBe` False
    isSingleGraphemeCluster "a" `shouldBe` False
    isSingleGraphemeCluster "👎🏻👎🏼👎🏽👎🏾👎🏿" `shouldBe` False
  it "endsInGraphemeCluster" $ do
    endsInGraphemeCluster "👎🏼" `shouldBe` True
    endsInGraphemeCluster "👎🏿" `shouldBe` True
    endsInGraphemeCluster "" `shouldBe` False
    endsInGraphemeCluster "a" `shouldBe` False
    endsInGraphemeCluster "👎🏻👎🏼👎🏽👎🏾👎🏿" `shouldBe` True
    endsInGraphemeCluster "👎🏻👎🏼👎🏽👎🏾👎🏿😱" `shouldBe` False
    endsInGraphemeCluster "goop😱dedoop👎🏿" `shouldBe` True
  it "removeGraphemeCluster" $ do
    removeGraphemeCluster "👎🏻👎🏼👎🏽👎🏾👎🏿" `shouldBe` "👎👎👎👎👎"
  it "containsGraphemeCluster" $ do
    containsGraphemeCluster "👎👎👎👎👎" `shouldBe` False
    containsGraphemeCluster "👎👎👎🏿👎👎👎" `shouldBe` True
    