
{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderedCanvasRegion(..)
  , renderedCanvas_box
  , renderedCanvasRegion_nonEmptyCount
  , emptyRenderedCanvasRegion
  , printRenderedCanvasRegion
  , potatoRender
  , potatoRenderPFState
  , render
  , renderedCanvasToText
  , renderedCanvasRegionToText

  , renderWithBroadPhase
  , moveRenderedCanvasRegion
  , updateCanvas

  -- exposed for testing
  , moveRenderedCanvasRegionNoReRender
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Types

import qualified Data.IntMap             as IM
import qualified Data.List.Ordered       as L
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed     as V
import qualified Data.Sequence as Seq
import Control.Exception (assert)


emptyChar :: PChar
emptyChar = ' '


-- TODO for selection rendering you want to make it V.Vector (Maybe PChar) or maybe you can just use a map?
{-
class IsRenderedCanvasRegion rc where
  isRenderedCanvasRegion_area :: LBox
  isRenderedCanvasRegion_generateMaybe :: (Int, Int) -> ((Int, Int) -> Maybe PChar) -> rc
-}

-- A rendered region in Canvas space
data RenderedCanvasRegion = RenderedCanvasRegion {
  _renderedCanvasRegion_box        :: LBox
  , _renderedCanvasRegion_contents :: V.Vector PChar -- ^ row major
} deriving (Eq, Show)

renderedCanvas_box :: RenderedCanvasRegion -> LBox
renderedCanvas_box = _renderedCanvasRegion_box

emptyRenderedCanvasRegion :: LBox -> RenderedCanvasRegion
emptyRenderedCanvasRegion lb@(LBox _ (V2 w h)) = RenderedCanvasRegion {
    _renderedCanvasRegion_box = lb
    , _renderedCanvasRegion_contents = V.replicate (w*h) emptyChar
  }

renderedCanvasRegion_nonEmptyCount :: RenderedCanvasRegion -> Int
renderedCanvasRegion_nonEmptyCount = V.length . V.filter (\x -> x /= emptyChar) . _renderedCanvasRegion_contents

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

-- | brute force renders a RenderedCanvasRegion
potatoRender :: [SElt] -> RenderedCanvasRegion -> RenderedCanvasRegion
potatoRender seltls RenderedCanvasRegion {..} = r where
  drawers = map getDrawer seltls
  genfn i = newc' where
    pt = toPoint _renderedCanvasRegion_box i
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d pt) drawers)
    newc' = case mdrawn of
      Just c  -> c
      Nothing -> ' '
  newc = V.generate (V.length _renderedCanvasRegion_contents) genfn
  r = RenderedCanvasRegion {
      _renderedCanvasRegion_box = _renderedCanvasRegion_box
      , _renderedCanvasRegion_contents = newc
    }

potatoRenderPFState :: OwlPFState -> RenderedCanvasRegion -> RenderedCanvasRegion
potatoRenderPFState OwlPFState {..} = potatoRender . fmap owlElt_toSElt_hack . fmap snd . toList . _owlTree_mapping $ _owlPFState_owlTree

-- TODO rewrite this so it can be chained and then take advantage of fusion
-- | renders just a portion of the RenderedCanvasRegion
-- caller is expected to provide all SElts that intersect the rendered LBox
render :: (HasCallStack) => LBox -> [SElt] -> RenderedCanvasRegion -> RenderedCanvasRegion
render llbx@(LBox (V2 x y) _) seltls RenderedCanvasRegion {..} = r where
  drawers = map getDrawer seltls
  genfn i = newc' where
    -- construct parent point and index
    pt@(V2 lx ly) = toPoint llbx i
    pindex = toIndex _renderedCanvasRegion_box pt
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d pt) drawers)
    -- render what we found or empty otherwise
    newc' = case mdrawn of
      Just c  -> (pindex, c)
      Nothing -> (pindex,emptyChar)
  -- go through each point in target LBox and render it
  newc = V.generate (lBox_area llbx) genfn
  r = RenderedCanvasRegion {
      _renderedCanvasRegion_box = _renderedCanvasRegion_box
      , _renderedCanvasRegion_contents = V.update _renderedCanvasRegion_contents newc
    }

renderedCanvasToText :: RenderedCanvasRegion -> Text
renderedCanvasToText RenderedCanvasRegion {..} = T.unfoldr unfoldfn (0, False) where
  l = V.length _renderedCanvasRegion_contents
  (LBox _ (V2 w _)) = _renderedCanvasRegion_box
  unfoldfn (i, eol) = if i == l
    then Nothing
    else if eol
      then Just $ ('\n', (i, False))
      else if (i+1) `mod` w == 0
        then Just $ (_renderedCanvasRegion_contents V.! i, (i+1, True))
        else Just $ (_renderedCanvasRegion_contents V.! i, (i+1, False))


-- TODO this does not handle wide chars at all fack
-- | assumes region LBox is strictly contained in _renderedCanvasRegion_box
renderedCanvasRegionToText :: LBox -> RenderedCanvasRegion -> Text
renderedCanvasRegionToText lbx RenderedCanvasRegion {..} = if not validBoxes then error ("render region outside canvas:\n" <> show lbx <> "\n" <> show _renderedCanvasRegion_box)
  else T.unfoldr unfoldfn (0, False) where

  validBoxes = intersect_lBox lbx _renderedCanvasRegion_box == Just lbx

  l = lBox_area lbx
  (LBox (V2 px py) _) = _renderedCanvasRegion_box
  (LBox _ (V2 lw _)) = lbx
  unfoldfn (i, eol) = if i == l
    then Nothing
    else if eol
      then Just $ ('\n', (i, False))
      else if (i+1) `mod` lw == 0
        then Just $ (_renderedCanvasRegion_contents V.! pindex, (i+1, True))
        else Just $ (_renderedCanvasRegion_contents V.! pindex, (i+1, False))
    where
      pt = toPoint lbx i
      pindex = toIndex _renderedCanvasRegion_box pt

printRenderedCanvasRegion :: RenderedCanvasRegion -> IO ()
printRenderedCanvasRegion rc@RenderedCanvasRegion {..} = T.putStrLn $ renderedCanvasRegionToText _renderedCanvasRegion_box rc

renderWithBroadPhase :: BPTree -> OwlTree -> LBox -> RenderedCanvasRegion -> RenderedCanvasRegion
renderWithBroadPhase bpt ot lbx rc = r where
  rids = broadPhase_cull lbx bpt
  sowls' = flip fmap rids $ \rid -> case owlTree_findSuperOwl ot rid of
      Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
      Just sowl -> sowl
  SuperOwlParliament sowls = makeSortedSuperOwlParliament ot $ Seq.fromList sowls'
  selts = fmap superOwl_toSElt_hack $ toList sowls
  r = render lbx selts rc

moveRenderedCanvasRegionNoReRender :: LBox -> RenderedCanvasRegion -> RenderedCanvasRegion
moveRenderedCanvasRegionNoReRender lbx RenderedCanvasRegion {..} = assert (area >= 0) outrcr where
  -- unnecessary to init with empty vector as moveRenderedCanvasRegion will re-render those areas
  -- but it's still nice to do and makes testing easier
  area = lBox_area lbx
  emptyv = V.replicate area ' '
  newv = case intersect_lBox lbx _renderedCanvasRegion_box of
    Just intersectlbx -> copiedv where
      (l,r,t,b) = lBox_to_axis intersectlbx
      -- [(newIndex, oldIndex)]
      indices' = [toIndexSafe _renderedCanvasRegion_box (V2 x y) >>= return . (toIndex lbx (V2 x y),) | x <- [l..(r-1)], y <- [t..(b-1)]]
      indices = catMaybes indices'
      indexedValues = fmap (\(idx, oldidx) -> (idx, _renderedCanvasRegion_contents V.! oldidx)) indices
      copiedv = (V.//) emptyv indexedValues
    Nothing -> emptyv

  outrcr = RenderedCanvasRegion {
      _renderedCanvasRegion_box = lbx
      , _renderedCanvasRegion_contents = newv
    }

-- TODO test
moveRenderedCanvasRegion :: BroadPhaseState -> OwlTree -> LBox -> RenderedCanvasRegion -> RenderedCanvasRegion
moveRenderedCanvasRegion (BroadPhaseState bpt) ot lbx rc = r where
  r1 = moveRenderedCanvasRegionNoReRender lbx rc
  r = foldr (\sublbx accrc -> renderWithBroadPhase bpt ot sublbx accrc) r1 (substract_lBox lbx (_renderedCanvasRegion_box rc))

-- TODO test
updateCanvas :: SuperOwlChanges -> NeedsUpdateSet -> BroadPhaseState -> OwlPFState -> RenderedCanvasRegion -> RenderedCanvasRegion
updateCanvas cslmap needsUpdate BroadPhaseState {..} OwlPFState {..} rc = case needsUpdate of
  [] -> rc
  -- TODO incremental rendering
  (b:bs) -> case intersect_lBox (renderedCanvas_box rc) (foldl' union_lBox b bs) of
    Nothing -> rc
    Just aabb -> r where
      rids' = broadPhase_cull aabb _broadPhaseState_bPTree
      --sortfn rid1 rid2 = compare (layerPosMap IM.! rid1) (layerPosMap IM.! rid2)
      -- TODO proper comparison function
      sortfn rid1 rid2 = compare rid1 rid2
      rids = L.sortBy sortfn rids'
      sowls' = flip fmap rids $ \rid -> case IM.lookup rid cslmap of
        Nothing -> case owlTree_findSuperOwl _owlPFState_owlTree rid of
          Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
          Just sowl -> sowl
        Just msowl -> case msowl of
          Nothing -> error "this should never happen, because deleted seltl would have been culled in broadPhase_cull"
          Just sowl -> sowl
      SuperOwlParliament sowls = makeSortedSuperOwlParliament _owlPFState_owlTree $ Seq.fromList sowls'
      selts = fmap superOwl_toSElt_hack $ toList sowls
      r = render aabb selts rc
