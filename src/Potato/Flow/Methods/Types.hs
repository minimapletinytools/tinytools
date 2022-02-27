{-# LANGUAGE RecordWildCards #-}

-- TODO rename to common
module Potato.Flow.Methods.Types where


import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import Potato.Flow.Owl

import qualified Data.Text          as T


class HasRenderCache a where
  hasRenderCache_renderCache :: a -> RenderCache

data RenderCache = RenderCache

emptyRenderCache :: RenderCache
emptyRenderCache = RenderCache

instance HasRenderCache OwlTree where
  hasRenderCache_renderCache _ = emptyRenderCache

type SEltDrawerRenderFn = forall a. (HasOwlTree a, HasRenderCache a) => a -> XY -> Maybe PChar
type SEltDrawerBoxFn = forall a. (HasOwlTree a, HasRenderCache a) => a -> LBox

makePotatoRenderer :: LBox -> SEltDrawerRenderFn
makePotatoRenderer lbox _ pt = if does_lBox_contains_XY lbox pt
  then Just '#'
  else Nothing

data SEltDrawer = SEltDrawer {

  -- TODO renameto boxFn
  _sEltDrawer_box        :: SEltDrawerBoxFn

  , _sEltDrawer_renderFn :: SEltDrawerRenderFn

  --, _sEltDrawer_renderToBoxFn :: LBox -> Vector PChar -- consider this version for better performance
}

nilDrawer :: SEltDrawer
nilDrawer = SEltDrawer {
    _sEltDrawer_box = const nilLBox
    , _sEltDrawer_renderFn = \_ _ -> Nothing
  }

sEltDrawer_renderToLines :: (HasOwlTree a, HasRenderCache a) => SEltDrawer -> a -> [Text]
sEltDrawer_renderToLines SEltDrawer {..} ot = r where
  LBox (V2 sx sy) (V2 w h) = _sEltDrawer_box ot
  pts = [[(x,y) | x <- [0..w-1]]| y <- [0..h-1]]
  r' = fmap (fmap (\(x,y) -> fromMaybe ' ' (_sEltDrawer_renderFn ot (V2 (sx+x) (sy+y))))) pts
  r = fmap T.pack r'


{-
TODO something like this
data CachedAreaDrawer = CachedAreaDrawer {
  _cachedAreaDrawer_box :: LBox
  , _cachedAreaDrawer_cache :: V.Vector (Maybe PChar) -- ^ row major
}-}



-- TODO rename to getSEltBoundingBox or something
-- | gets an 'LBox' that contains the entire RElt
getSEltBox :: SElt -> Maybe LBox
getSEltBox selt = case selt of
  SEltNone        -> Nothing
  SEltFolderStart -> Nothing
  SEltFolderEnd   -> Nothing
  -- TODO return canonical
  SEltBox x       -> Just $ canonicalLBox_from_lBox_ $ _sBox_box x
  SEltLine x      -> Just $ union_lBox
    (make_lBox_from_XYs (_sSimpleLine_start x) (_sSimpleLine_end x))
    (make_lBox_from_XYs (_sSimpleLine_start x + 1) (_sSimpleLine_end x + 1))
  SEltTextArea x      -> Just $ canonicalLBox_from_lBox_ $ _sTextArea_box x

getSEltLabelBox :: SEltLabel -> Maybe LBox
getSEltLabelBox (SEltLabel _ x) = getSEltBox x
