{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderedCanvas
  , renderedCanvas_box
  , emptyRenderedCanvas
  , potatoRender
  , render
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

toIndex :: LBox -> XY -> Int
toIndex (LBox (V2 x y) (V2 w _)) (V2 px py) = py*w+px

-- | brute force renders a RenderedCanvas
potatoRender :: [SElt] -> RenderedCanvas -> RenderedCanvas
potatoRender seltls RenderedCanvas {..} = r where
  drawers = reverse $ map getDrawer seltls
  genfn i = newc' where
    pt = toPoint _renderedCanvas_box i
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d pt) drawers)
    newc' = case mdrawn of
      Just c  -> c
      Nothing -> ' '
  newc = V.generate (V.length _renderedCanvas_contents) genfn
  r = RenderedCanvas {
      _renderedCanvas_box = _renderedCanvas_box
      , _renderedCanvas_contents = newc
    }


-- TODO rewrite this so it can be chained and then take advantage of fusion
-- | renders just a portion of the RenderedCanvas
render :: LBox -> [SElt] -> RenderedCanvas -> RenderedCanvas
render llbx@(LBox (V2 x y) _) seltls RenderedCanvas {..} = r where
  drawers = reverse $ map getDrawer seltls
  genfn i = newc' where
    -- construct parent point and index
    lpt@(V2 lx ly) = toPoint llbx i
    ppt = V2 (lx+x) (ly+y)
    pindex = toIndex _renderedCanvas_box ppt
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d ppt) drawers)
    newc' = case mdrawn of
      Just c  -> (pindex, c)
      Nothing -> (pindex,' ')
  newc = V.generate (lBox_area llbx) genfn
  r = RenderedCanvas {
      _renderedCanvas_box = _renderedCanvas_box
      , _renderedCanvas_contents = V.update _renderedCanvas_contents newc
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
