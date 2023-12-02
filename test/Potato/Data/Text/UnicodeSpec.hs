{-# LANGUAGE OverloadedStrings #-}

module Potato.Data.Text.UnicodeSpec (
  spec
) where

import           Prelude

import           Test.Hspec

import           Control.Monad

import           Potato.Data.Text.Unicode


doubleWidthChars :: String
doubleWidthChars = "ｔｈｅｒｅ　ａｒｅ　ｎｏ　ｓｐａｃｅ　ｂｅｔｗｅｅｎ　ａｄｊａｃｅｎｔ　ｃｈａｒａｃｔｅｒｓ"

spec :: Spec
spec = describe "Unicode" $ do
  it "isSingleGraphemeCluster" $ do
    isSingleGraphemeCluster "👎🏼" `shouldBe` True
    isSingleGraphemeCluster "👎" `shouldBe` False
    isSingleGraphemeCluster "" `shouldBe` False
    isSingleGraphemeCluster "a" `shouldBe` False
    isSingleGraphemeCluster "👎🏻👎🏼👎🏽👎🏾👎🏿" `shouldBe` False
    isSingleGraphemeCluster "🧑\8205🍳" `shouldBe` True -- zwidges can't be used directly https://gitlab.haskell.org/ghc/ghc/-/issues/21228
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
  forM_ doubleWidthChars $ \c -> do
    it ("getCharWidth " <> show c) $ do
      getCharWidth c `shouldBe` 2
