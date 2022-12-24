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

tz = TextZipper (reverse ["this is an example content of", "a text zipper"])
                "the capital "
                ["TEXT IS THE SELECTED", "PORTION"]
                " of the"
                ["text zipper"]

spec :: Spec
spec =
  describe "Zipper2" $ do

  it "leftN 1" $ do
    let tz_1 = TextZipper (reverse ["this is an example content of", "a text zipper"])
                        "the capital"
                        []
                        " TEXT IS THE SELECTED"
                        ["PORTION of the","text zipper"]
    leftN 1 tz `shouldBe` tz_1

  it "leftN 5" $ do
    let tz_5 = TextZipper   (reverse ["this is an example content of", "a text zipper"])
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

  it "home/end/top/up example_1" $ do
    let tz_0 = TextZipper  ["this is an example"]
                            "this line is selected "
                            []
                            "lines after"
                            ["blah blah blah"]
        tz_1h = TextZipper  ["this is an example"]
                            ""
                            []
                            "this line is selected lines after"
                            ["blah blah blah"]
        tz_1e = TextZipper  ["this is an example"]
                            "this line is selected lines after"
                            []
                            ""
                            ["blah blah blah"]
        tz_1t = TextZipper  []
                            ""
                            []
                            "this is an example"
                            ["this line is selected lines after", "blah blah blah"]
        tz_1u = TextZipper  []
                            "this is an example"
                            []
                            "this line is selected lines after"
                            ["blah blah blah"]
    home tz_0 `shouldBe` tz_1h
    end tz_0 `shouldBe` tz_1e
    top tz_0 `shouldBe` tz_1t
--    up tz_0 `shouldBe` tz_1u

  it "home/end example_2" $ do
    let tz_0 = TextZipper  ["this is an example"]
                    "this line is selected "
                    ["pooh bear"]
                    " lines after"
                    ["blah blah blah"]
        tz_1h = TextZipper  ["this is an example"]
                    ""
                    []
                    "this line is selected pooh bear lines after"
                    ["blah blah blah"]
        tz_1e = TextZipper  ["this is an example"]
                            "this line is selected pooh bear lines after"
                            []
                            ""
                            ["blah blah blah"]
    home tz_0 `shouldBe` tz_1h
    end tz_0 `shouldBe` tz_1e
    
  it "home example_3" $ do
    let tz_0 = TextZipper  ["this is an example"]
                    "this line is selected "
                    ["pooh bear", "canned chicken cat"]
                    " lines after"
                    ["blah blah blah"]
        tz_1h = TextZipper  ["this is an example"]
                    ""
                    []
                    "this line is selected pooh bear"
                    ["canned chicken cat lines after", "blah blah blah"]
        tz_1e = TextZipper  ["this is an example", "this line is selected pooh bear"]
                    "canned chicken cat lines after"
                    []
                    ""
                    ["blah blah blah"]
    home tz_0 `shouldBe` tz_1h
    end tz_0 `shouldBe` tz_1e
