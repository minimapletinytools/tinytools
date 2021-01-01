{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.RenderSpec(
  spec
) where

import           Relude                 hiding (empty, fromList)

import           Test.Hspec

import           Data.Default           (def)
import qualified Data.IntMap            as IM
import qualified Data.Text              as T

import           Potato.Flow
import           Potato.Flow.TestStates

testCanvas :: Int -> Int -> Int -> Int -> RenderedCanvas
testCanvas x y w h = emptyRenderedCanvas (LBox (V2 x y) (V2 w h))

spec :: Spec
spec = do
  describe "Canvas" $ do
    it "potato renders blank text" $ do
      let
        (w,h) = (1003, 422)
        canvasText = renderedCanvasToText (testCanvas (-540) 33 w h)
      T.length canvasText `shouldBe` w * h + h - 1
    it "potato renders stuff" $ do
      let
        canvas1 = testCanvas (-12) (-44) 100 100
        n = 10
        selts = flip map [1..n] $ \i ->
          SEltBox $ SBox {
              _sBox_box    = LBox (V2 (i*2) 0) (V2 2 2)
              , _sBox_style = def
            }
        canvas2 = potatoRender selts canvas1
        canvas2Text = renderedCanvasToText canvas2
      --putTextLn $ canvas2Text
      T.length (T.filter (\x -> x /= ' ' && x /= '\n') canvas2Text) `shouldBe` n*4
    it "renders negative LBox" $ do
      let
        canvas1 = testCanvas 0 0 20 20
        selt = SEltBox $ SBox {
            _sBox_box    = LBox (V2 10 10) (V2 (-10) (-10))
            , _sBox_style = def
          }
        canvas2 = potatoRender [selt] canvas1
        canvas2Text = renderedCanvasToText canvas2
      T.length (T.filter (\x -> x /= ' ' && x /= '\n') canvas2Text) `shouldBe` 100
    it "renders to a region" $ do
      let
        fillBox = LBox (V2 (-12) (-44)) (V2 100 100)
        renderBox = LBox (V2 (-1) 10) (V2 10 10)
        canvas1 = emptyRenderedCanvas fillBox
        selt = SEltBox $ SBox {
            _sBox_box    = fillBox
            , _sBox_style = def
          }
        canvas2 = render renderBox [selt] canvas1
        canvas2Text = renderedCanvasToText canvas2
        canvas2TextRegion = renderedCanvasRegionToText renderBox canvas2
      --putTextLn $ canvas2Text
      T.length (T.filter (\x -> x /= ' ' && x /= '\n') canvas2Text) `shouldBe` lBox_area renderBox
      T.length (T.filter (\x -> x /= ' ' && x /= '\n') canvas2TextRegion) `shouldBe` lBox_area renderBox
    it "moveRenderedCanvasNoReRender" $ do
      let
        -- fill the whole canvas
        canvas1 = testCanvas 0 0 100 100
        selt = SEltBox $ SBox {
            _sBox_box    = LBox (V2 0 0) (V2 100 100)
            , _sBox_style = def
          }
        canvas2 = potatoRender [selt] canvas1
        target = LBox (V2 (-50) (-50)) (V2 100 100)
        canvas3 = moveRenderedCanvasNoReRender target canvas2
        canvas3Text = renderedCanvasToText canvas3
      T.length (T.filter (\x -> x /= ' ' && x /= '\n') canvas3Text) `shouldBe` 50*50
    it "updateCanvas - basic" $ do
      let
        makeChange rid lb = IM.singleton rid $ Just (SEltLabel (show rid) (SEltBox $ SBox lb def def def False))
        canvas0 = testCanvas 0 0 100 100
        state0 = pfstate_basic1
        bpt0 = bPTreeFromPFState state0
        -- TODO actual changes
        changes1 = IM.empty
        bps1 = update_bPTree IM.empty bpt0
        state1 = state0
        canvas1 = updateCanvas changes1 bps1 state1 (pFState_getLayerPosMap state1) canvas0
      -- TODO test something
      0 `shouldBe` 0
