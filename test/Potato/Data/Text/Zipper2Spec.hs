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

  it "home/end example_1" $ do
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
    home tz_0 `shouldBe` tz_1h
    end tz_0 `shouldBe` tz_1e

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
    
  it "home/end example_3" $ do
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

  it "top examples" $ do
    top (TextZipper [] "" [] "dd d" ["a","b"]) `shouldBe` 
         TextZipper [] "" [] "dd d" ["a","b"]
    top (TextZipper ["hello"] "" [] "dd d" ["a","b"]) `shouldBe` 
         TextZipper [] "" [] "hello" ["dd d", "a","b"]
    top (TextZipper (reverse ["hello", "ok", "very good"]) "" [] "dd d" ["a","b"]) `shouldBe` 
         TextZipper [] "" [] "hello" ["ok", "very good","dd d", "a","b"]
    top (TextZipper (reverse ["hello", "ok", "very good"]) "" [] "dd d" ["a","b"]) `shouldBe` 
         TextZipper [] "" [] "hello" ["ok", "very good","dd d", "a","b"]
    top (TextZipper (reverse ["hello", "ok", "very good"]) "" ["a b c"] "dd d" ["a","b"]) `shouldBe` 
         TextZipper [] "" [] "hello" ["ok", "very good","a b cdd d", "a","b"]
    top (TextZipper (reverse ["hello", "ok", "very good"]) "" ["a b c", "d e "] "dd d" ["a","b"]) `shouldBe` 
         TextZipper [] "" [] "hello" ["ok", "very good", "a b c", "d e dd d", "a","b"]

  it "up examples" $ do
    up (TextZipper [] "ab " [] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper [] "" [] "ab dd d" ["a","b"])
    up (TextZipper ["cat dog"] "ab " [] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper [] "cat" [] " dog" ["ab dd d", "a","b"])
    up (TextZipper ["cat dog"] "abfeeeere " [] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper [] "cat dog" [] "" ["abfeeeere dd d", "a","b"])
    up (TextZipper (reverse ["cat dog", "bird_sky"]) "ab " [] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper ["cat dog"] "bir" [] "d_sky" ["ab dd d", "a","b"])
    up (TextZipper [] "ab " ["story"] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper [] "" [] "ab storydd d" ["a","b"])
    up (TextZipper [] "ab " ["story", "cool"] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper [] "" [] "ab story" ["cooldd d", "a","b"])
{-
b
a
ab S|TORY
COOLdd d
a
b
-}
    up (TextZipper ["a", "b"] "ab " ["STORY", "COOL"] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper ["a", "b"] "ab S" [] "TORY" ["cooldd d", "a","b"])
        -- (TextZipper ["a", "b"] "" [] "ab story" ["cooldd d", "a","b"])
    up (TextZipper (reverse ["a", "b"]) "ab " ["story"] "dd d" ["a","b"]) `shouldBe` 
        (TextZipper ["a"] "b" [] "" ["ab storydd d", "a","b"])

  it "down example_1" $ do
    down (TextZipper ["a","b"] "ab " [] "dd d" []) `shouldBe` 
        (TextZipper ["a", "b"] "ab dd d" [] "" [])
  it "down example_2" $ do
    down (TextZipper (reverse ["a","b"]) "ab " [] "dd d" ["hello"]) `shouldBe` 
        (TextZipper (reverse ["a", "b", "ab dd d"]) "hel" [] "lo" [])
  it "down example_3" $ do
    down (TextZipper (reverse ["a","b"]) "ab " [] "dd d" ["hello", "thanks"]) `shouldBe` 
        (TextZipper (reverse ["a", "b", "ab dd d"]) "hel" [] "lo" ["thanks"])
  it "down example_4" $ do
{-
a
b
ab CENTERdd d|
-}
    down (TextZipper (reverse ["a", "b"]) "ab " ["CENTER"] "dd d" []) `shouldBe` 
         (TextZipper (reverse ["a", "b"]) "ab CENTERdd d" [] "" [])
  it "down example_5" $ do
    down (TextZipper (reverse ["a","b"]) "ab " ["center"] "dd d" ["hello", "thanks"]) `shouldBe` 
        (TextZipper (reverse ["a", "b", "ab centerdd d"]) "hello" [] "" ["thanks"])
  it "down example_6" $ do
{-
a
b
ab CENTER
MIDDLE dd d
hello|
thanks
-}
    down (TextZipper (reverse ["a", "b"]) "ab " ["CENTER", "MIDDLE"] " dd d" ["hello", "thanks"]) `shouldBe`
         (TextZipper (reverse ["a", "b", "ab CENTER"]) "hello" [] "" ["thanks"] )
         

















