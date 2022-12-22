{-# LANGUAGE OverloadedStrings #-}

module Potato.Data.Text.Zipper2Spec (
  spec
) where

import           Prelude

import           Test.Hspec

import qualified Data.Map         as Map
import qualified          Data.Text as T
import Control.Monad

import           Potato.Data.Text.Zipper2

tz_ = TextZipper ["this is an example content of", "a text zipper"]
                "the capital "
                ["TEXT IS THE SELECTED", "PORTION"]
                " of the"
                ["text zipper"]

tz = TextZipper ["a text zipper", "this is an example content of"]
                "the capital "
                ["TEXT IS THE SELECTED", "PORTION"]
                " of the"
                ["text zipper"]

spec :: Spec
spec =
  describe "Zipper2" $ do
  it "leftN 1" $ do
    let tz_1 = TextZipper   ["a text zipper", "this is an example content of"]
                        "the capital"
                        []
                        " TEXT IS THE SELECTED"
                        ["PORTION of the","text zipper"]
    leftN 1 tz `shouldBe` tz_1

  it "leftN 5" $ do
    let tz_5 = TextZipper   ["a text zipper", "this is an example content of"]
                            "the cap"
                            []
                            "ital TEXT IS THE SELECTED"
                            ["PORTION of the","text zipper"]
    leftN 5 tz `shouldBe` tz_5

  it "leftN 20" $ do
    let tz_20 = TextZipper  ["this is an example content of"]
                            "a text"
                            []
                            " zipper"
                            ["the capital TEXT IS THE SELECTED","PORTION of the","text zipper"]
    leftN 20 tz `shouldBe` tz_20

  it "home example_1" $ do
    let tz_0 = TextZipper  ["this is an example"]
                            "this line is selected "
                            []
                            "lines after"
                            ["blah blah blah"]
        tz_1 = TextZipper  ["this is an example"]
                            ""
                            []
                            "this line is selected lines after"
                            ["blah blah blah"]
    home tz_0 `shouldBe` tz_1

  it "home example_2" $ do
    let tz_0 = TextZipper  ["this is an example"]
                    "this line is selected "
                    ["pooh bear"]
                    " lines after"
                    ["blah blah blah"]
        tz_1 = TextZipper  ["this is an example"]
                    ""
                    []
                    "this line is selected pooh bear lines after"
                    ["blah blah blah"]
    home tz_0 `shouldBe` tz_1
    
  it "home example_3" $ do
    let tz_0 = TextZipper  ["this is an example"]
                    "this line is selected "
                    ["pooh bear", "canned chicken cat"]
                    " lines after"
                    ["blah blah blah"]
        tz_1 = TextZipper  ["this is an example"]
                    ""
                    []
                    "this line is selected pooh bear"
                    ["canned chicken cat lines after", "blah blah blah"]
    home tz_0 `shouldBe` tz_1
