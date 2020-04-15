{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}


module Potato.Flow.Reflex.Layers (
  LayerPos
  , LayerView
  , LayerElt(..)
  , LayerTree(..)
  , LayerTreeConfig(..)
) where

import           Relude

import           Reflex

type LayerPos = Int

class LayerElt a where
  type LayerEltId a :: Type
  isFolderStart :: a -> Bool
  isFolderEnd :: a -> Bool


-- must satisfy the scoping property
type LayerView a = [LayerEltId a]

data LayerTree t a = LayerTree {
  _layerTree_view         :: Dynamic t (LayerView a)
  , _layerTree_changeView :: Dynamic t (PatchMap (LayerEltId a) a) -- only elements that were added, moved, or deleted

  -- does this really need to be piped through LayerTree?
  , _layerTree_copied     :: Dynamic t (LayerView a)
}

data LayerTreeConfig t a = LayerTreeConfig {
  _layerTreeConfig_add         :: Event t (LayerEltId a, a)
  , _layerTreeConfig_remove    :: Event t (LayerEltId a)
  -- todo need to generalize for many elts
  , _layerTreeConfig_move      :: Event t (LayerPos, LayerPos) -- (from, to)
  , _layerTreeConfig_copy      :: Event t [LayerEltId a]
  -- maybe change to addMany?
  --, _layerTreeConfig_paste :: Event t []
  , _layerTreeConfig_duplicate :: Event t [LayerEltId a]

}
