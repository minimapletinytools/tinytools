{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderedCanvas
  , renderedCanvas_box
  , emptyRenderedCanvas
  , potatoRender
  , renderedCanvasToText
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts

import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import qualified Data.Vector.Unboxed      as V


data RenderedCanvas = RenderedCanvas {
  _renderedCanvas_box        :: LBox
  , _renderedCanvas_contents :: V.Vector PChar -- ^ row major
}

renderedCanvas_box :: RenderedCanvas -> LBox
renderedCanvas_box = _renderedCanvas_box

emptyRenderedCanvas :: LBox -> RenderedCanvas
emptyRenderedCanvas lb@(LBox _ (V2 w h)) = RenderedCanvas {
    _renderedCanvas_box = lb
    , _renderedCanvas_contents = V.replicate (w*h) ' '
  }

toPoint :: LBox -> Int -> XY
toPoint (LBox (V2 x y) (V2 w _)) i = V2 (i `mod` w + y) (i `div` w + x)

-- | brute force renders a RenderedCanvas
potatoRender :: [SElt] -> RenderedCanvas -> RenderedCanvas
potatoRender seltls RenderedCanvas {..} = r where
  drawers = reverse $ map getDrawer seltls
  genfn i = newc' where
    pt = toPoint _renderedCanvas_box i
    -- go through drawers in reverse order until you find a match
    mdraw = find (\d -> isJust $ _sEltDrawer_renderFn d pt) drawers
    newc' = case mdraw of
      Just d  -> fromJust $ _sEltDrawer_renderFn d pt
      Nothing -> ' '
  newc = V.generate (V.length _renderedCanvas_contents) genfn
  r = RenderedCanvas {
      _renderedCanvas_box = _renderedCanvas_box
      , _renderedCanvas_contents = newc
    }

renderedCanvasToText :: RenderedCanvas -> Text
renderedCanvasToText RenderedCanvas {..} = T.unfoldr unfoldfn (0, False) where
  l = V.length _renderedCanvas_contents
  (LBox _ (V2 w _)) = _renderedCanvas_box
  unfoldfn (i, eol) = if i == l
    then Nothing
    else if eol
      then Just $ ('\n', (i, False))
      else if (i+1) `mod` w == 0
        then Just $ (_renderedCanvas_contents V.! i, (i+1, True))
        else Just $ (_renderedCanvas_contents V.! i, (i+1, False))
