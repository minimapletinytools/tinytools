{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Potato.Flow.SEltMethodsSpec(
  spec
) where

import           Relude       hiding (empty, fromList)

import           Test.Hspec

import qualified Data.Map as Map
import           Data.Default (def)

import           Potato.Flow


spec :: Spec
spec = do
  let
    testsstyle = def { _superStyle_fill = FillStyle_Simple '@' }
    renderfn sd = _sEltDrawer_renderFn sd emptyOwlTree
  describe "SEltMethod" $ do
    describe "getDrawerFromSEltForTest" $ do
      describe "SBox" $ do
        let
          somesbox1 style = def {
              _sBox_box       = LBox 0 (V2 5 5)
              , _sBox_text    = def {
                  _sBoxText_text = "m ow meow meow"
                }
              , _sBox_boxType = style
              , _sBox_style = testsstyle
            }
          somesbox2 style = def {
              _sBox_box       = LBox 0 (V2 1 1)
              , _sBox_boxType = style
              , _sBox_style = testsstyle
            }
          somesbox3 style = def {
              _sBox_box       = LBox 0 (V2 10 10)
              , _sBox_text    = def {
                  _sBoxText_text = "m ow meeuew ee  meow hello boop no12 meow whatever"
                  , _sBoxText_style = TextStyle TextAlign_Right
                }
              , _sBox_boxType = style
              , _sBox_style = testsstyle
            }
          somesbox4 style = def {
              _sBox_box       = LBox 0 (V2 10 10)
              , _sBox_text    = def {
                  _sBoxText_text = "ｔｈｅｒｅ　ａｒｅ　ｎｏ　ｓｐａｃｅ　ｂｅｔｗｅｅｎ　ａｄｊａｃｅｎｔ　ｃｈａｒａｃｔｅｒｓ"
                  , _sBoxText_style = TextStyle TextAlign_Left
                }
              , _sBox_boxType = style
              , _sBox_style = testsstyle
            }
          somesbox5 style = def {
            _sBox_box       = LBox 0 (V2 10 5)
            , _sBox_text    = def {
                _sBoxText_text = "aoeuaoeu"
              }
            , _sBox_title = SBoxTitle {
                _sBoxTitle_title = Just "boop"
                , _sBoxTitle_align = TextAlign_Right
              }
            , _sBox_boxType = style
            , _sBox_style = testsstyle
          }
        it "SBoxType_NoBoxText" $ do
          let
            sd = getDrawerFromSEltForTest (SEltBox (somesbox1 SBoxType_NoBoxText))
          renderfn sd (V2 100 0) `shouldBe` Nothing
          renderfn sd (V2 (-1) 0) `shouldBe` Nothing
          renderfn sd (V2 0 0) `shouldBe` Just 'm'
          renderfn sd (V2 1 0) `shouldBe` Just ' '
          renderfn sd (V2 2 0) `shouldBe` Just 'o'
          -- TODO it seems EOL space characters are not rendered and this gives the fill char. Maybe this is desired behavior thought?
          --renderfn sd (V2 3 0) `shouldBe` Just ' '
          renderfn sd (V2 1 1) `shouldBe` Just 'e'
          renderfn sd (V2 4 4) `shouldBe` Just '@'
        it "SBoxType_Box" $ do
          let
            sd = getDrawerFromSEltForTest (SEltBox (somesbox2 SBoxType_Box))
          --forM_ (sEltDrawer_renderToLines sd emptyOwlTree) putTextLn
          renderfn sd (V2 0 0) `shouldBe` _superStyle_point def
        it "SBoxType_NoBoxText_alignRight" $ do
          let
            sd = getDrawerFromSEltForTest (SEltBox (somesbox3 SBoxType_NoBoxText))
          --forM_ (sEltDrawer_renderToLines sd emptyOwlTree) putTextLn
          renderfn sd (V2 0 0) `shouldBe` Just '@'
        it "SBoxType_NoBoxText_widechar" $ do
          let
            sd = getDrawerFromSEltForTest (SEltBox (somesbox4 SBoxType_NoBoxText))
          --forM_ (sEltDrawer_renderToLines sd emptyOwlTree) putTextLn
          renderfn sd (V2 0 0) `shouldBe` Just 'ｔ'
          renderfn sd (V2 1 0) `shouldBe` Nothing
          renderfn sd (V2 2 0) `shouldBe` Just 'ｈ'
          renderfn sd (V2 3 0) `shouldBe` Nothing
        it "box label" $ do
          let
            sd = getDrawerFromSEltForTest (SEltBox (somesbox5 SBoxType_BoxText))
          --forM_ (sEltDrawer_renderToLines sd emptyOwlTree) putTextLn
          renderfn sd (V2 0 0) `shouldBe` Just '╔'
          renderfn sd (V2 8 0) `shouldBe` Just 'p'
      describe "STextArea" $ do
        let
          sometextarea = STextArea {
              _sTextArea_box         = LBox (V2 1 1) (V2 10 10)
              , _sTextArea_text      = Map.fromList [((V2 0 0),'a'),((V2 1 1),'b'),((V2 9 9),'c')]
              , _sTextArea_transparent = False
            }
        it "basic" $ do
          let
            sd = getDrawerFromSEltForTest (SEltTextArea $ sometextarea)
          --forM_ ((sEltDrawer_renderToLines sd) emptyOwlTree sd) putTextLn
          renderfn sd (V2 1 1) `shouldBe` Just 'a'
          renderfn sd (V2 10 10) `shouldBe` Just 'c'
          renderfn sd (V2 100 0) `shouldBe` Nothing
