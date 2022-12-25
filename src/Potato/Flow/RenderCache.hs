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
