{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.Types (
  RenderFn
  , makePotatoRenderer
  , SEltDrawer(..)
  , sEltDrawer_renderToLines
  , nilDrawer
) where


import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import qualified Data.Text          as T


type RenderFn = XY -> Maybe PChar

makePotatoRenderer :: LBox -> RenderFn
makePotatoRenderer lbox pt = if does_lBox_contains_XY lbox pt
  then Just '#'
  else Nothing

data SEltDrawer = SEltDrawer {
  _sEltDrawer_box        :: LBox
  , _sEltDrawer_renderFn :: RenderFn -- switch to [RenderFn] for better performance

  --, _sEltDrawer_renderToBoxFn :: LBox -> Vector PChar -- consider this version for better performance
}

nilDrawer :: SEltDrawer
nilDrawer = SEltDrawer {
    _sEltDrawer_box = nilLBox
    , _sEltDrawer_renderFn = const Nothing
  }

sEltDrawer_renderToLines :: SEltDrawer -> [Text]
sEltDrawer_renderToLines SEltDrawer {..} = r where
  LBox (V2 sx sy) (V2 w h) = _sEltDrawer_box
  pts = [[(x,y) | x <- [0..w-1]]| y <- [0..h-1]]
  r' = fmap (fmap (\(x,y) -> fromMaybe ' ' (_sEltDrawer_renderFn (V2 (sx+x) (sy+y))))) pts
  r = fmap T.pack r'
