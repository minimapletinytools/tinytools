
{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  RenderCache(..)
  , RenderContext(..)
  , emptyRenderContext
  , emptyRenderCache
  , renderCache_clearAtKeys
  , renderCache_lookup
  , render -- TODO DELETE use render_new instead because it uses cache

  , RenderedCanvasRegion(..)
  , renderedCanvas_box
  , renderedCanvasRegion_nonEmptyCount
  , emptyRenderedCanvasRegion
  , printRenderedCanvasRegion
  , potatoRenderWithOwlTree
  , potatoRenderPFState
  , renderedCanvasToText
  , renderedCanvasRegionToText

  , renderWithBroadPhase
  , moveRenderedCanvasRegion
  , updateCanvas

  -- exposed for testing
  , moveRenderedCanvasRegionNoReRender
) where

import           Relude

import           Potato.Flow.RenderCache
import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.Methods.Types
import           Potato.Flow.SElts
import Potato.Flow.Types
import Potato.Flow.OwlState
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.Controller.Types
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Methods.LineTypes


import qualified Data.IntMap             as IM
import qualified Data.Text               as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed     as V
import qualified Data.Sequence as Seq
import Control.Exception (assert)

-- rather pointless abstraction but it's useful to have during refactors such that I don't ned to provide an explicit LayerMetaMap
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

-- RenderContext is a helper container type that provides both read and write data for various render operations
data RenderContext = RenderContext {
  _renderContext_cache :: RenderCache -- r/w
  , _renderContext_owlTree :: OwlTree -- r
  , _renderContext_layerMetaMap :: LayerMetaMap -- r
  , _renderContext_broadPhase :: BroadPhaseState -- r
  , _renderContext_renderedCanvasRegion :: RenderedCanvasRegion -- r/w
}

emptyRenderContext :: LBox -> RenderContext
emptyRenderContext lbox = RenderContext {
    _renderContext_cache = emptyRenderCache
    , _renderContext_owlTree = emptyOwlTree
    , _renderContext_layerMetaMap = IM.empty
    , _renderContext_broadPhase = emptyBroadPhaseState
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
  , _renderedCanvasRegion_contents :: V.Vector MWidePChar -- ^ row major
} deriving (Eq, Show)

renderedCanvas_box :: RenderedCanvasRegion -> LBox
renderedCanvas_box = _renderedCanvasRegion_box

emptyRenderedCanvasRegion :: LBox -> RenderedCanvasRegion
emptyRenderedCanvasRegion lb@(LBox _ (V2 w h)) = RenderedCanvasRegion {
    _renderedCanvasRegion_box = lb
    , _renderedCanvasRegion_contents = V.replicate (w*h) emptyMWidePChar
  }

-- empty spaces due to wide chars to the left are not counted
renderedCanvasRegion_nonEmptyCount :: RenderedCanvasRegion -> Int
renderedCanvasRegion_nonEmptyCount = V.length . V.filter (\x -> x /= emptyMWidePChar) . _renderedCanvasRegion_contents

-- | brute force renders a RenderedCanvasRegion (ignores broadphase)
potatoRenderWithOwlTree :: OwlTree -> [OwlSubItem] -> RenderedCanvasRegion -> RenderedCanvasRegion
potatoRenderWithOwlTree ot osubitems prevrcr = r where
  drawerswithcache = fmap (\osubitem -> (getDrawerWithCache osubitem Nothing, Nothing)) osubitems 
  llbx = _renderedCanvasRegion_box prevrcr
  r = render_withCache ot llbx drawerswithcache prevrcr

potatoRenderPFState :: OwlPFState -> RenderedCanvasRegion
potatoRenderPFState OwlPFState {..} = potatoRenderWithOwlTree _owlPFState_owlTree (fmap _owlItem_subItem . fmap snd . toList . _owlTree_mapping $ _owlPFState_owlTree) (emptyRenderedCanvasRegion (_sCanvas_box _owlPFState_canvas))


render_withCache :: (HasOwlTree a) => a -> LBox -> [(SEltDrawer, Maybe OwlItemCache)] -> RenderedCanvasRegion -> RenderedCanvasRegion
render_withCache ot llbx drawerswithcache prevrcr = r where

  genfn i = newc' where
    -- construct parent point and index
    pt = toPoint llbx i
    pindex = toIndex (_renderedCanvasRegion_box prevrcr) pt

    -- go through drawers in reverse order until you find a match
    --mdrawn = join . find isJust $ (fmap (\d -> _sEltDrawer_renderFn d _renderContext_owlTree pt) drawers)
    -- go through caches (they should have all been updated in the previous step) until you find a match
    drawfn (drawer, mcache) = case mcache of
      Nothing -> drawnocache
      --Just _ -> drawnocache
      Just cache -> case owlItemCache_preRender cache of
        Nothing -> drawnocache
        Just pr -> case preRender_lookup pr pt of
          (-1, _) -> Nothing
          x       -> Just x
      where
        drawnocache = case _sEltDrawer_renderFn drawer ot pt of
          Nothing -> Nothing
          Just x  -> Just (0, x)
    mdrawn = join . find isJust $ fmap drawfn drawerswithcache

    -- render what we found or empty otherwise
    newc' = case mdrawn of
      Just c  -> (pindex, c)
      Nothing -> (pindex, emptyMWidePChar)

  -- go through each point in target LBox and render it
  newc = V.generate (lBox_area llbx) genfn
  r = prevrcr {
      _renderedCanvasRegion_contents = V.update (_renderedCanvasRegion_contents prevrcr) newc
    }


-- TODO DELETE use render_new instead
-- | renders just a portion of the RenderedCanvasRegion
-- caller is expected to provide all SElts that intersect the rendered LBox (broadphase is ignored)
-- SElts are rendered in ORDER
render :: LBox -> [OwlSubItem] -> RenderContext -> RenderContext
render llbx osubitems rctx@RenderContext {..} = r where
  drawerswithcache = fmap (\osubitem -> (getDrawerWithCache osubitem Nothing, Nothing)) osubitems 
  prevrcr = _renderContext_renderedCanvasRegion
  r = rctx {
      _renderContext_renderedCanvasRegion = render_withCache _renderContext_owlTree llbx drawerswithcache prevrcr
    }


-- | renders just a portion of the RenderedCanvasRegion
-- updates cache as appropriate
-- caller is expected to provide all REltIds that intersect the rendered LBox (broadphase is ignored)
-- REltIds are rendered in ORDER
render_new :: LBox -> [REltId] -> RenderContext -> RenderContext
render_new llbx rids rctx@RenderContext {..} = rctxout where
  prevrcr = _renderContext_renderedCanvasRegion

  -- TODO split cache update into a separate function
  mapaccumlfn cacheacc rid = r where
    -- see if it was in the previous cache
    mprevcache = IM.lookup rid (unRenderCache _renderContext_cache)
    sowl = owlTree_mustFindSuperOwl _renderContext_owlTree rid
    OwlItem _ osubitem = _superOwl_elt sowl
    mupdatedcache = updateOwlSubItemCache _renderContext_owlTree osubitem
    r = case mprevcache of
      Just c -> (cacheacc, (osubitem, Just c))
      Nothing -> case mupdatedcache of
        Just c -> (IM.insert rid c cacheacc, (osubitem, Just c))
        Nothing -> (cacheacc, (osubitem, Nothing))

  (newcache, owlswithcache) = mapAccumL mapaccumlfn (unRenderCache _renderContext_cache) rids
  drawerswithcache = map (\(x, c)-> (getDrawerWithCache x c, c)) owlswithcache

  rctxout = rctx {
      _renderContext_cache = RenderCache newcache
      , _renderContext_renderedCanvasRegion = render_withCache _renderContext_owlTree llbx drawerswithcache prevrcr
    }


renderedCanvasToText :: RenderedCanvasRegion -> Text
renderedCanvasToText rcr = renderedCanvasRegionToText (_renderedCanvasRegion_box rcr) rcr


-- TODO this does not handle wide chars at all fack
-- | assumes region LBox is strictly contained in _renderedCanvasRegion_box
renderedCanvasRegionToText :: LBox -> RenderedCanvasRegion -> Text
renderedCanvasRegionToText lbx RenderedCanvasRegion {..} = if not validBoxes then error ("render region outside canvas:\n" <> show lbx <> "\n" <> show _renderedCanvasRegion_box)
  else r where

  validBoxes = intersect_lBox_include_zero_area lbx _renderedCanvasRegion_box == Just lbx

  l = lBox_area lbx
  (LBox _ (V2 lw _)) = lbx
  unfoldfn (i, eol) = if i == l
    then Nothing
    else if eol
      then Just $ ('\n', (i, False))
      else Just $ (pchar, (i+1, neweol))
      where
        neweol = (i+1) `mod` lw == 0
        (_, pchar) = _renderedCanvasRegion_contents V.! pindex
        pt = toPoint lbx i
        pindex = toIndex _renderedCanvasRegion_box pt
  r' = T.unfoldr unfoldfn (0, False)

  -- TODO need to remove extra characters following wide char
  -- or better yet do preloop to remove wide characters before unfoldring your output text
  r = r'

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
  sortedrids = fmap _superOwl_id $ toList sowls

  r = render_new lbx sortedrids rctx

moveRenderedCanvasRegionNoReRender :: LBox -> RenderedCanvasRegion -> RenderedCanvasRegion
moveRenderedCanvasRegionNoReRender lbx RenderedCanvasRegion {..} = assert (area >= 0) outrcr where
  -- unnecessary to init with empty vector as moveRenderedCanvasRegion will re-render those areas
  -- but it's still nice to do and makes testing easier
  area = lBox_area lbx
  emptyv = V.replicate area emptyMWidePChar
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
  prevrc = _renderContext_renderedCanvasRegion
  rctx1 = rctx {
      _renderContext_renderedCanvasRegion = moveRenderedCanvasRegionNoReRender lbx prevrc
    }
  r = foldr renderWithBroadPhase rctx1 (substract_lBox lbx (_renderedCanvasRegion_box prevrc))

updateCanvas :: SuperOwlChanges -> NeedsUpdateSet -> RenderContext -> RenderContext
updateCanvas cslmap needsupdateaabbs rctx@RenderContext {..} = case needsupdateaabbs of
  [] -> rctx
  -- TODO create disjoint union of all boxes and render than one at a time instead union_lBoxing them all
  --aoeu@(b:bs) -> trace "UPDATE CANVAS" $ traceShow aoeu $ case intersect_lBox (renderedCanvas_box _renderContext_renderedCanvasRegion) (foldl' union_lBox b bs) of
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
      sortedrids = fmap _superOwl_id $ toList sowls
      r = render_new aabb sortedrids rctx
