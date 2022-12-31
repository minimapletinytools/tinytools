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
  it "removeGraphemeCluster" $ do
    removeGraphemeCluster "👎🏻👎🏼👎🏽👎🏾👎🏿" `shouldBe` "👎👎👎👎👎"
    