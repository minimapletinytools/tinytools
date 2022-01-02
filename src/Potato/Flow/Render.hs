
{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderCache(..)
  , RenderContext(..)
  , emptyRenderContext
  , emptyRenderCache

  , RenderedCanvasRegion(..)
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
  , toIndexSafe
  , moveRenderedCanvasRegionNoReRender
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.Methods.Types
import           Potato.Flow.SElts
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Controller.Types
import Potato.Flow.Controller.Manipulator.Select

import qualified Data.IntMap             as IM
import qualified Data.Text               as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed     as V
import qualified Data.Sequence as Seq
import Control.Exception (assert)

-- rather pointless abstraction but it's useful to have during refactors
class OwlRenderSet a where
  findSuperOwl :: a -> REltId -> Maybe (SuperOwl, Bool)
  sortForRendering :: a -> Seq.Seq SuperOwl -> Seq.Seq SuperOwl
  findSuperOwlForRendering :: a -> REltId -> Maybe SuperOwl
  findSuperOwlForRendering ors rid = case findSuperOwl ors rid of
    Nothing -> Nothing
    Just (sowl, b) -> if b then Nothing else Just sowl

instance OwlRenderSet OwlTree where
  findSuperOwl ot = fmap (,False) . owlTree_findSuperOwl ot
  sortForRendering a sowls = unSuperOwlParliament $ makeSortedSuperOwlParliament a sowls

instance OwlRenderSet (OwlTree, LayerMetaMap) where
  findSuperOwl (ot,lmm) rid = r where
    hidden = layerMetaMap_isInheritHidden ot rid lmm
    r = fmap (,hidden) $ owlTree_findSuperOwl ot rid
  sortForRendering (ot,_) sowls = sortForRendering ot sowls



data RenderCache = RenderCache

emptyRenderCache :: RenderCache
emptyRenderCache = RenderCache

-- includes helper methods needed for render but not what to render
data RenderContext = RenderContext {
  _renderContext_owlTree :: OwlTree
  , _renderContext_layerMetaMap :: LayerMetaMap
  , _renderContext_broadPhase :: BroadPhaseState

  -- TODO use me
  , _renderContext_cache :: RenderCache
  
  , _renderContext_renderedCanvasRegion :: RenderedCanvasRegion
}

emptyRenderContext :: LBox -> RenderContext
emptyRenderContext lbox = RenderContext {
    _renderContext_owlTree = emptyOwlTree
    , _renderContext_layerMetaMap = IM.empty
    , _renderContext_broadPhase = emptyBroadPhaseState
    , _renderContext_cache = emptyRenderCache
    , _renderContext_renderedCanvasRegion = emptyRenderedCanvasRegion lbox
  }


instance HasOwlTree RenderContext where
  hasOwlTree_owlTree = hasOwlTree_owlTree . _renderContext_owlTree

instance OwlRenderSet RenderContext where
  findSuperOwl RenderContext {..} rid = findSuperOwl (_renderContext_owlTree, _renderContext_layerMetaMap) rid
  sortForRendering RenderContext {..} sowls = sortForRendering (_renderContext_owlTree, _renderContext_layerMetaMap) sowls



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

-- TODO you need to pass in OwlTree for this to render attachments correctly
-- | brute force renders a RenderedCanvasRegion (ignores broadphase)
potatoRender :: [SElt] -> RenderedCanvasRegion -> RenderedCanvasRegion
potatoRender seltls RenderedCanvasRegion {..} = r where
  drawers = map getDrawer seltls
  genfn i = newc' where
    pt = toPoint _renderedCanvasRegion_box i
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d emptyOwlTree pt) drawers)
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
-- caller is expected to provide all SElts that intersect the rendered LBox (broadphase is ignored)
render :: LBox -> [SElt] -> RenderContext -> RenderContext
render llbx seltls rctx@RenderContext {..} = r where
  prevrcr = _renderContext_renderedCanvasRegion
  drawers = map getDrawer seltls
  genfn i = newc' where
    -- construct parent point and index
    pt = toPoint llbx i
    pindex = toIndex (_renderedCanvasRegion_box prevrcr) pt
    -- go through drawers in reverse order until you find a match
    mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d _renderContext_owlTree pt) drawers)
    -- render what we found or empty otherwise
    newc' = case mdrawn of
      Just c  -> (pindex, c)
      Nothing -> (pindex,emptyChar)
  -- go through each point in target LBox and render it
  newc = V.generate (lBox_area llbx) genfn
  r = rctx {
      -- TODO update cache
      _renderContext_renderedCanvasRegion = prevrcr {
          _renderedCanvasRegion_contents = V.update (_renderedCanvasRegion_contents prevrcr) newc
        }
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

  validBoxes = intersect_lBox_include_zero_area lbx _renderedCanvasRegion_box == Just lbx

  l = lBox_area lbx
  (LBox (V2 _ _) _) = _renderedCanvasRegion_box
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

renderWithBroadPhase :: LBox -> RenderContext -> RenderContext
renderWithBroadPhase  lbx rctx@RenderContext {..} = r where
  bpt = (_broadPhaseState_bPTree _renderContext_broadPhase)
  rids = broadPhase_cull lbx bpt

  -- TODO I THINK THIS IS INCORRECT DELETE, specifically, broadPhase_cull will give hidden elements that we can't find with findSuperOwlForRendering
  {-sowls' = flip fmap rids $ \rid -> case findSuperOwlForRendering ot rid of
      Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
      Just sowl -> sowl-}
  sowls' = catMaybes $ fmap (\rid -> findSuperOwlForRendering _renderContext_owlTree rid) rids

  sowls = sortForRendering _renderContext_owlTree $ Seq.fromList sowls'
  selts = fmap superOwl_toSElt_hack $ toList sowls

  r = render lbx selts rctx

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

moveRenderedCanvasRegion ::  LBox -> RenderContext -> RenderContext
moveRenderedCanvasRegion lbx rctx@RenderContext {..} = r where
  bpt = (_broadPhaseState_bPTree _renderContext_broadPhase)
  prevrc = _renderContext_renderedCanvasRegion
  rctx1 = rctx {
      _renderContext_renderedCanvasRegion = moveRenderedCanvasRegionNoReRender lbx prevrc
    }
  r = foldr renderWithBroadPhase rctx1 (substract_lBox lbx (_renderedCanvasRegion_box prevrc))

updateCanvas :: SuperOwlChanges -> NeedsUpdateSet -> RenderContext -> RenderContext
updateCanvas cslmap needsupdateaabbs rctx@RenderContext {..} = case needsupdateaabbs of
  [] -> rctx
  -- TODO incremental rendering
  (b:bs) -> case intersect_lBox (renderedCanvas_box _renderContext_renderedCanvasRegion) (foldl' union_lBox b bs) of
    Nothing -> rctx
    Just aabb -> r where
      rids = broadPhase_cull aabb (_broadPhaseState_bPTree _renderContext_broadPhase)

      msowls = flip fmap rids $ \rid -> case IM.lookup rid cslmap of
        Nothing -> case findSuperOwl _renderContext_owlTree rid of
          Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
          -- changes could indicate hidden, if that's the case, give a dummy object to render
          Just (sowl, hidden) -> if hidden then Nothing else Just sowl
        Just msowl -> case msowl of
          Nothing -> error "this should never happen, because deleted seltl would have been culled in broadPhase_cull"
          Just sowl -> Just sowl
      sowls = sortForRendering _renderContext_owlTree $ Seq.fromList (catMaybes msowls)
      selts = fmap superOwl_toSElt_hack $ toList sowls
      r = render aabb selts rctx
