{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.RenderSpec(
  spec
) where

import           Relude                   hiding (empty, fromList)

import           Test.Hspec

import qualified Data.Text                as T
import Data.Default (def)

import           Potato.Flow

testCanvas :: Int -> Int -> Int -> Int -> Canvas
testCanvas x y w h = emptyCanvas (LBox (LPoint (V2 x y)) (LSize (V2 w h)))

spec :: Spec
spec = do
  describe "Canvas" $ do
    it "renders blank text" $ do
      let
        (w,h) = (1003, 422)
        canvasText = canvasToText (testCanvas (-540) 33 w h)
      T.length canvasText `shouldBe` w * h + h - 1
    it "draws stuff" $ do
      let
        canvas1 = testCanvas (-12) (-44) 100 100
        n = 10
        selts = flip map [1..n] $ \i ->
          SEltBox $ SBox {
              _sBox_box    = LBox (LPoint (V2 (i*2) 0)) (LSize (V2 2 2))
              , _sBox_style = def
            }
        canvas2 = potatoRender selts canvas1
        canvas2Text = canvasToText canvas2
      --putTextLn $ canvas2Text
      -- TODO update for real renderer
      T.length (T.filter (\x -> x /= ' ' && x /= '\n') canvas2Text) `shouldBe` n*4
