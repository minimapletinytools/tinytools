{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderedCanvas
  , renderedCanvas_box
  , emptyRenderedCanvas
  , potatoRender
  , render
  , renderedCanvasToText
  , renderedCanvasRegionToText

  , moveRenderedCanvas
  , updateCanvas
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import qualified Data.Vector.Unboxed     as V


emptyChar :: PChar
emptyChar = ' '

-- TODO rename, this should mean the portion of the screen that is rendered, not the canvas
data RenderedCanvas = RenderedCanvas {
  _renderedCanvas_box        :: LBox
  , _renderedCanvas_contents :: V.Vector PChar -- ^ row major
} deriving (Show)

renderedCanvas_box :: RenderedCanvas -> LBox
renderedCanvas_box = _renderedCanvas_box

emptyRenderedCanvas :: LBox -> RenderedCanvas
emptyRenderedCanvas lb@(LBox _ (V2 w h)) = RenderedCanvas {
    _renderedCanvas_box = lb
    , _renderedCanvas_contents = V.replicate (w*h) ' '
  }

-- | input index must be contained in the box
toPoint :: LBox -> Int -> XY
toPoint (LBox (V2 x y) (V2 w _)) i = V2 (i `mod` w + x) (i `div` w + y)

-- | input XY point must be contained in the box
toIndex :: LBox -> XY -> Int
toIndex (LBox (V2 x y) (V2 w _)) (V2 px py) = (py-y)*w+(px-x)

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
-- caller is expected to provide all SElts that intersect the rendered LBox
render :: (HasCallStack) => LBox -> [SElt] -> RenderedCanvas -> RenderedCanvas
render llbx@(LBox (V2 x y) _) seltls RenderedCanvas {..} = r where
  drawers = reverse $ map getDrawer seltls
  genfn i = newc' where
    -- construct parent point and index
    pt@(V2 lx ly) = toPoint llbx i
    pindex = toIndex _renderedCanvas_box pt
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d pt) drawers)
    -- render what we found or empty otherwise
    newc' = case mdrawn of
      Just c  -> (pindex, c)
      Nothing -> (pindex,emptyChar)
  -- go through each point in target LBox and render it
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

-- | assumes region LBox is strictly contained in _renderedCanvas_box
renderedCanvasRegionToText :: LBox -> RenderedCanvas -> Text
renderedCanvasRegionToText lbx RenderedCanvas {..} = T.unfoldr unfoldfn (0, False) where
  l = lBox_area lbx
  (LBox (V2 px py) _) = _renderedCanvas_box
  (LBox _ (V2 lw _)) = lbx
  unfoldfn (i, eol) = if i == l
    then Nothing
    else if eol
      then Just $ ('\n', (i, False))
      else if (i+1) `mod` lw == 0
        then Just $ (_renderedCanvas_contents V.! pindex, (i+1, True))
        else Just $ (_renderedCanvas_contents V.! pindex, (i+1, False))
    where
      pt = toPoint lbx i
      pindex = toIndex _renderedCanvas_box pt

moveRenderedCanvasNoReRender :: LBox -> RenderedCanvas -> RenderedCanvas
moveRenderedCanvasNoReRender (LBox pt@(V2 xt yt) (V2 wt ht)) RenderedCanvas {..} = r where
  -- TODO
  r' = RenderedCanvas {
      _renderedCanvas_box = undefined
      , _renderedCanvas_contents = undefined
    }
  r = r'

moveRenderedCanvas :: LBox -> BroadPhaseState -> PFState -> RenderedCanvas -> RenderedCanvas
moveRenderedCanvas = undefined

updateCanvas :: SEltLabelChanges -> BroadPhaseState -> PFState -> RenderedCanvas -> RenderedCanvas
updateCanvas = undefined
