{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.RenderCache where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import           Potato.Flow.SElts
import Potato.Flow.Types
import Potato.Flow.Owl
import           Potato.Flow.Methods.LineTypes


import qualified Data.IntMap             as IM
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed     as V
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

emptyMWidePChar :: MWidePChar 
emptyMWidePChar = (-1, ' ') -- ' ' is a dummy character needed to pad the unboxed vector

-- TODO consider making sparse variant
-- | the LBox may exceed the logical bounding box of the object that is being represented if that object contains wide chars
data PreRender = PreRender (V.Vector (MWidePChar)) LBox deriving (Show)

emptyPreRender :: PreRender
emptyPreRender = PreRender V.empty (LBox 0 0)

preRender_lookup :: (HasCallStack) => PreRender -> XY -> MWidePChar
preRender_lookup (PreRender v lbox) pos = r where
  -- we still have to do this check here since toIndex expects point to be contained in box
  r = if does_lBox_contains_XY lbox pos 
    then case v V.!? (toIndex lbox pos) of
      Nothing -> assert False emptyMWidePChar
      Just x -> x
    else emptyMWidePChar


-- NOTE OwlIteCache is intended to be used at several stages in the event loop
-- it can be used in Handlers, it can be used when generating SEltDrawers and it can be used for rendering itself
data OwlItemCache =
  -- TODO change to LineAnchorsForRenderList prob
  OwlItemCache_Line LineAnchorsForRender PreRender
  | OwlItemCache_Generic PreRender deriving (Show)

owlItemCache_preRender :: OwlItemCache -> Maybe PreRender
owlItemCache_preRender = \case
  OwlItemCache_Line _ x -> Just x
  OwlItemCache_Generic x -> Just x

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


-- UNTESTED
makePreRender :: forall a. (HasOwlTree a) => a -> SEltDrawer -> PreRender
makePreRender ot SEltDrawer {..} = r where
  
  lbox' = _sEltDrawer_box ot 
  lbox@(LBox _ (V2 w _)) = lBox_expand lbox' (0, _sEltDrawer_maxCharWidth-1, 0, 0)
  area = lBox_area lbox

  -- the (Int8, Int8, PChar) is (distance from prev wide pchar, width of prev wide pchar, wide pchar)
  -- width of prev wide pchar could be determined from wide pchar of course but may as well cache it
  unfoldrfn :: (Int, Maybe (Int8, Int8, PChar)) -> Maybe (MWidePChar, (Int, Maybe (Int8, Int8, PChar)))
  unfoldrfn (i, mprevwidechar) = r2 where
    pt = toPoint lbox i
    mchar = _sEltDrawer_renderFn ot pt
    mcharwithwidth = fmap (\pch -> (pch, getPCharWidth pch)) mchar
    eol = (i+1) `mod` w == 0

    moutputchar = case mprevwidechar of
      Nothing        -> case mchar of  
        Just pch -> (0, pch)
        Nothing -> emptyMWidePChar 
      Just (a,_,pch) -> (a, pch)

    mnextwidechar = if eol 
      then Nothing 
      else case mprevwidechar of 
        Nothing -> case mcharwithwidth of 
          Nothing -> Nothing
          Just (pch, width) -> if width > 1
            then Just (1,width,pch)
            else Nothing
        Just (a,b,_) | a == b-1 -> Nothing
        Just (a,b,pch) -> Just (a+1, b, pch)

    r2 = if i == area 
      then Nothing
      else Just (moutputchar, (i+1, mnextwidechar))

  r = PreRender (V.unfoldr unfoldrfn (0,Nothing)) lbox


