{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.RenderCache where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import           Potato.Flow.SElts
import Potato.Flow.Types
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.Controller.Types
import           Potato.Flow.Methods.LineTypes


import qualified Data.IntMap             as IM
import qualified Data.Text               as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed     as V
import qualified Data.Sequence as Seq
import Control.Exception (assert)

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


-- | WidePChar represents part of a PChar that
-- the Int8 parameter is offset from where the PChar originates from, so for example
-- '😱' <- PChar
--  01 <- Int8 offset parameter
--
-- -1 value for offset means there is no character in the space, the PChar value is ignored in this case
type MWidePChar = (Int8, PChar)

-- TODO consider making sparse variant
data PreRender = PreRender (V.Vector (MWidePChar)) LBox deriving (Show)

emptyPreRender :: PreRender
emptyPreRender = PreRender V.empty (LBox 0 0)

preRender_lookup :: PreRender -> XY -> MWidePChar
preRender_lookup (PreRender v lbox) pos = assert (does_lBox_contains_XY lbox pos) $ v V.! (toIndex lbox pos)


data OwlItemCache =
  -- TODO change to LineAnchorsForRenderList prob
  OwlItemCache_Line LineAnchorsForRender PreRender
  | OwlItemCache_Generic PreRender deriving (Show)

newtype RenderCache = RenderCache {
    -- map for REltId to cache for each owl
    unRenderCache :: REltIdMap OwlItemCache
  } deriving (Show)

emptyRenderCache :: RenderCache
emptyRenderCache = RenderCache IM.empty

renderCache_clearAtKeys :: RenderCache -> [REltId] -> RenderCache
renderCache_clearAtKeys rcache rids = RenderCache $ foldr IM.delete (unRenderCache rcache) rids

renderCache_lookup :: RenderCache -> REltId -> Maybe OwlItemCache
renderCache_lookup rcache rid = IM.lookup rid (unRenderCache rcache)

-- TODO
--makePreRender :: SEltDrawer -> PreRender
--makePreRender SEltDrawer {..} = r where
