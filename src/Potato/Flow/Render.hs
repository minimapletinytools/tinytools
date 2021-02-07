{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderedCanvas(..)
  , renderedCanvas_box
  , emptyRenderedCanvas
  , printRenderedCanvas
  , potatoRender
  , potatoRenderPFState
  , render
  , renderedCanvasToText
  , renderedCanvasRegionToText

  , renderWithBroadPhase
  , moveRenderedCanvas
  , updateCanvas

  -- exposed for testing
  , moveRenderedCanvasNoReRender
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import qualified Data.IntMap             as IM
import qualified Data.List.Ordered       as L
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed     as V
import Control.Exception (assert)


emptyChar :: PChar
emptyChar = ' '

-- TODO rename, this should mean the portion of the screen that is rendered, not the canvas
data RenderedCanvas = RenderedCanvas {
  _renderedCanvas_box        :: LBox
  , _renderedCanvas_contents :: V.Vector PChar -- ^ row major
} deriving (Eq, Show)

renderedCanvas_box :: RenderedCanvas -> LBox
renderedCanvas_box = _renderedCanvas_box

emptyRenderedCanvas :: LBox -> RenderedCanvas
emptyRenderedCanvas lb@(LBox _ (V2 w h)) = RenderedCanvas {
    _renderedCanvas_box = lb
    , _renderedCanvas_contents = V.replicate (w*h) ' '
  }

-- TODO move these methods to Math
-- | input index must be contained in the box
toPoint :: LBox -> Int -> XY
toPoint (LBox (V2 x y) (V2 w _)) i = V2 (i `mod` w + x) (i `div` w + y)

-- | input XY point must be contained in the box
toIndex :: LBox -> XY -> Int
toIndex (LBox (V2 x y) (V2 w _)) (V2 px py) = (py-y)*w+(px-x)

-- | same as above but does bounds checking
toIndexSafe :: LBox -> XY -> Maybe Int
toIndexSafe lbx xy = if does_lBox_contains_XY lbx xy
  then Just $ toIndex lbx xy
  else Nothing

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

potatoRenderPFState :: PFState -> RenderedCanvas -> RenderedCanvas
potatoRenderPFState PFState {..} = potatoRender (fmap (\(SEltLabel _ selt) -> selt) $ toList _pFState_directory)

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


-- TODO this does not handle wide chars at all fack
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

printRenderedCanvas :: RenderedCanvas -> IO ()
printRenderedCanvas rc@RenderedCanvas {..} = T.putStrLn $ renderedCanvasRegionToText _renderedCanvas_box rc

renderWithBroadPhase :: BPTree -> REltIdMap SEltLabel -> LBox -> RenderedCanvas -> RenderedCanvas
renderWithBroadPhase bpt dir lbx rc = r where
  rids = broadPhase_cull lbx bpt
  seltls = flip fmap rids $ \rid -> case IM.lookup rid dir of
      Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
      Just seltl -> _sEltLabel_sElt seltl
  r = render lbx seltls rc

moveRenderedCanvasNoReRender :: LBox -> RenderedCanvas -> RenderedCanvas
moveRenderedCanvasNoReRender lbx RenderedCanvas {..} = assert (area >= 0) r where
  -- unnecessary to init with empty vector as moveRenderedCanvas will re-render those areas
  -- but it's still nice to do and makes testing easier
  area = lBox_area lbx
  emptyv = V.replicate area ' '
  newv = case intersect_lBox lbx _renderedCanvas_box of
    Just intersectlbx -> copiedv where
      (l,r,t,b) = lBox_to_axis intersectlbx
      -- [(newIndex, oldIndex)]
      indices' = [toIndexSafe _renderedCanvas_box (V2 x y) >>= return . (toIndex lbx (V2 x y),) | x <- [l..(r-1)], y <- [t..(b-1)]]
      indices = catMaybes indices'
      indexedValues = fmap (\(idx, oldidx) -> (idx, _renderedCanvas_contents V.! oldidx)) indices
      copiedv = (V.//) emptyv indexedValues
    Nothing -> emptyv

  r = RenderedCanvas {
      _renderedCanvas_box = lbx
      , _renderedCanvas_contents = newv
    }

-- TODO test
moveRenderedCanvas :: BroadPhaseState -> REltIdMap SEltLabel -> LBox -> RenderedCanvas -> RenderedCanvas
moveRenderedCanvas (BroadPhaseState bpt) dir lbx rc = r where
  r1 = moveRenderedCanvasNoReRender lbx rc
  r = foldr (\sublbx accrc -> renderWithBroadPhase bpt dir sublbx accrc) r1 (substract_lBox lbx (_renderedCanvas_box rc))

-- TODO test
updateCanvas :: SEltLabelChanges -> NeedsUpdateSet -> BroadPhaseState -> PFState -> LayerPosMap -> RenderedCanvas -> RenderedCanvas
updateCanvas cslmap needsUpdate BroadPhaseState {..} PFState {..} layerPosMap rc = case needsUpdate of
  [] -> rc
  -- TODO incremental rendering
  (b:bs) -> case intersect_lBox (renderedCanvas_box rc) (foldl' union_lBox b bs) of
    Nothing -> rc
    Just aabb -> r where
      rids' = broadPhase_cull aabb _broadPhaseState_bPTree
      sortfn rid1 rid2 = compare (layerPosMap IM.! rid1) (layerPosMap IM.! rid2)
      rids = L.sortBy sortfn rids'
      seltls = flip fmap rids $ \rid -> case IM.lookup rid cslmap of
        Nothing -> case IM.lookup rid _pFState_directory of
          Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
          Just seltl -> seltl
        Just mseltl -> case mseltl of
          Nothing -> error "this should never happen, because deleted seltl would have been culled in broadPhase_cull"
          Just seltl -> seltl
      r = render aabb (map _sEltLabel_sElt seltls) rc
