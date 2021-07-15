{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SEltMethodsSpec(
  spec
) where

import           Relude       hiding (empty, fromList)

import           Test.Hspec

import           Data.Default (def)

import           Potato.Flow


spec :: Spec
spec = do
  describe "SEltMethod" $ do
    describe "getDrawer" $ do
      describe "SBox" $ do
        let
          somesbox1 style = def {
              _sBox_box       = LBox 0 (V2 5 5)
              , _sBox_text    = def {
                  _sBoxText_text = "m ow meow meow"
                }
              , _sBox_boxType = style
            }
          somesbox2 style = def {
              _sBox_box       = LBox 0 (V2 1 1)
              , _sBox_boxType = style
            }
          somesbox3 style = def {
              _sBox_box       = LBox 0 (V2 10 10)
              , _sBox_text    = def {
                  _sBoxText_text = "m ow meeuew ee  meow hello boop no12 meow whatever"
                  , _sBoxText_style = TextStyle TextAlign_Right
                }
              , _sBox_boxType = style
            }
          somesbox4 style = def {
              _sBox_box       = LBox 0 (V2 10 10)
              , _sBox_text    = def {
                  _sBoxText_text = "ｔｈｅｒｅ　ａｒｅ　ｎｏ　ｓｐａｃｅ　ｂｅｔｗｅｅｎ　ａｄｊａｃｅｎｔ　ｃｈａｒａｃｔｅｒｓ"
                  , _sBoxText_style = TextStyle TextAlign_Left
                }
              , _sBox_boxType = style
            }
        it "SBoxType_NoBoxText" $ do
          let
            SEltDrawer {..} = getDrawer (SEltBox (somesbox1 SBoxType_NoBoxText))
          _sEltDrawer_renderFn (V2 100 0) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 (-1) 0) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 0 0) `shouldBe` Just 'm'
          _sEltDrawer_renderFn (V2 1 0) `shouldBe` Just ' '
          _sEltDrawer_renderFn (V2 2 0) `shouldBe` Just 'o'
          -- TODO it seems EOL space characters are not rendered and this gives the fill char. Maybe this is desired behavior thought?
          --_sEltDrawer_renderFn (V2 3 0) `shouldBe` Just ' '
          _sEltDrawer_renderFn (V2 1 1) `shouldBe` Just 'e'
          _sEltDrawer_renderFn (V2 4 4) `shouldBe` Just '@'
        it "SBoxType_Box" $ do
          let
            sd@SEltDrawer {..} = getDrawer (SEltBox (somesbox2 SBoxType_Box))
          --forM_ (sEltDrawer_renderToLines sd) putTextLn
          _sEltDrawer_renderFn (V2 0 0) `shouldBe` _superStyle_point def
        it "SBoxType_NoBoxText_alignRight" $ do
          let
            sd@SEltDrawer {..} = getDrawer (SEltBox (somesbox3 SBoxType_NoBoxText))
          --forM_ (sEltDrawer_renderToLines sd) putTextLn
          _sEltDrawer_renderFn (V2 0 0) `shouldBe` Just '@'
        it "SBoxType_NoBoxText_widechar" $ do
          let
            sd@SEltDrawer {..} = getDrawer (SEltBox (somesbox4 SBoxType_NoBoxText))
          --forM_ (sEltDrawer_renderToLines sd) putTextLn
          _sEltDrawer_renderFn (V2 0 0) `shouldBe` Just 'ｔ'
          _sEltDrawer_renderFn (V2 1 0) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 2 0) `shouldBe` Just 'ｈ'
          _sEltDrawer_renderFn (V2 3 0) `shouldBe` Nothing
      describe "SSimpleLine" $ do
        let
          somelinestyle autoStyle = LineStyle {
              _lineStyle_leftArrows    = "<="
              , _lineStyle_rightArrows = "->"
              , _lineStyle_upArrows    = "^|*"
              , _lineStyle_downArrows  = "V"
              , _lineStyle_autoStyle   = autoStyle
            }
          someline1 autoStyle = SSimpleLine {
              _sSimpleLine_start       = V2 10 10
              , _sSimpleLine_end       = V2 20 20
              , _sSimpleLine_style     = def
              , _sSimpleLine_lineStyle = somelinestyle autoStyle
            }
          someline2 autoStyle = SSimpleLine {
              _sSimpleLine_start       = V2 10 10
              , _sSimpleLine_end       = V2 10 20
              , _sSimpleLine_style     = def
              , _sSimpleLine_lineStyle = somelinestyle autoStyle
            }
          someline3 autoStyle = SSimpleLine {
              _sSimpleLine_start       = V2 5 20
              , _sSimpleLine_end       = V2 10 10
              , _sSimpleLine_style     = def
              , _sSimpleLine_lineStyle = somelinestyle autoStyle
            }
        it "LineAutoStyle_AutoStraight - 1" $ do
          let
            sd@SEltDrawer {..} = getDrawer (SEltLine $ someline1 LineAutoStyle_AutoStraight)
          --forM_ (sEltDrawer_renderToLines sd) putTextLn
          _sEltDrawer_renderFn (V2 10 10) `shouldBe` Just '<'
          _sEltDrawer_renderFn (V2 11 10) `shouldBe` Just '='
          _sEltDrawer_renderFn (V2 16 10) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 12 10) `shouldBe` _superStyle_horizontal def
          _sEltDrawer_renderFn (V2 15 10) `shouldBe` _superStyle_tr def
          _sEltDrawer_renderFn (V2 15 15) `shouldBe` _superStyle_vertical def
          _sEltDrawer_renderFn (V2 20 20) `shouldBe` Just '>'
        it "LineAutoStyle_AutoStraight - 2" $ do
          let
            sd@SEltDrawer {..} = getDrawer (SEltLine $ someline2 LineAutoStyle_AutoStraight)
          --forM_ (sEltDrawer_renderToLines sd) putTextLn
          _sEltDrawer_renderFn (V2 10 10) `shouldBe` Just '^'
          _sEltDrawer_renderFn (V2 10 12) `shouldBe` Just '*'
          _sEltDrawer_renderFn (V2 16 10) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 10 20) `shouldBe` Just 'V'
          _sEltDrawer_renderFn (V2 10 15) `shouldBe` _superStyle_vertical def
        it "LineAutoStyle_AutoStraight - 3" $ do
          let
            sd@SEltDrawer {..} = getDrawer (SEltLine $ someline3 LineAutoStyle_AutoStraight)
          --forM_ (sEltDrawer_renderToLines sd) putTextLn
          _sEltDrawer_renderFn (V2 10 10) `shouldBe` Just '^'
          _sEltDrawer_renderFn (V2 9 10) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 10 14) `shouldBe` _superStyle_vertical def
          _sEltDrawer_renderFn (V2 10 15) `shouldBe` _superStyle_br def
