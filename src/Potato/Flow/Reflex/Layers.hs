{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}


module Potato.Flow.Reflex.Layers (
  LayerPos
  , LayerView
  , isValidLayerView
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
  getId :: a -> LayerEltId a

type LayerView a = [a]

-- TODO test
-- | checks if 'LayerView' satisfies scoping property
isValidLayerView :: (LayerElt a) => LayerView a -> Bool
isValidLayerView lv = foldl' foldfn 0 lv == 0 where
  foldfn scope elt = (+) scope $ if isFolderStart elt
    then 1
    else if isFolderEnd elt then (-1)
    else 0

-- | layer tree does not provide an interface to ensure scoping property is satisfied
-- if inputs are correct, than outputs are correct, so corroctly use correct outputs to ensure inputs are always correct!
data LayerTree t a = LayerTree {
  _layerTree_view         :: Dynamic t (LayerView a)
  , _layerTree_changeView :: Dynamic t (PatchMap (LayerEltId a) a) -- only elements that were added, moved, or deleted

  -- does this really need to be piped through LayerTree?
  , _layerTree_copied     :: Dynamic t (LayerView a)
}

data LayerTreeConfig t a = LayerTreeConfig {
  -- | ensure input 'LayerView' satsifies scoping property
  -- if 'LayerPos' is out of range, results in error
  _layerTreeConfig_add      :: Event t (LayerPos, LayerView a)
  -- | ensure removing elements does not break scoping property
  , _layerTreeConfig_remove :: Event t [LayerEltId a]
  -- | ensure copied elts satisfy scoping property
  , _layerTreeConfig_copy   :: Event t [LayerEltId a]

  -- TODO this is a little weird with scoping
  -- ensure duplicated elts satisfy scoping property
  --, _layerTreeConfig_duplicate :: Event t [LayerEltId a]

  -- TODO make this work with many elements
  --, _layerTreeConfig_move      :: Event t (LayerPos, LayerPos) -- (from, to)

}
