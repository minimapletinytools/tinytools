{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Layers (

) where

import           Relude

import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import qualified Data.IntMap       as IM

-- DELETE prob
data LayerEntry = LayerEntry {
  _layerEntry_indent           :: Int
  , _layerEntry_display        :: Text
  , _layerEntry_isFolder       :: Bool

  -- stuff above is redundant if we have this but whatever
  , _layerEntry_superSEltLabel :: SuperSEltLabel

}
generateLayers :: PFState -> [Int]
generateLayers PFState {..} = r where
  foldrfn rid idents = newDepth:idents where
    seltl = case IM.lookup rid _pFState_directory of
      Nothing -> error "invalid PFState"
      Just x  -> x
    depth = case idents of
      []  -> 0
      x:_ -> x
    newDepth = case seltl of
      SEltLabel _ SEltFolderStart -> depth - 1
      SEltLabel _ SEltFolderEnd   -> depth + 1
  r = foldr foldrfn [] _pFState_layers
