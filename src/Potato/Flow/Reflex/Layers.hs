{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE RecursiveDo             #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Potato.Flow.Reflex.Layers (
  LayerPos
  , LayerView
  , isValidLayerView
  , LayerElt(..)
  , LayerTree(..)
  , LayerTreeConfig(..)
  , layerTree_attachEndPos
  , layerTree_tagEndPos
  , holdLayerTree
) where

import           Relude
import           Relude.Extra.Map

import           Reflex
import           Reflex.Data.Sequence
import           Reflex.Potato.Helpers

import           Control.Exception     (assert)
import           Control.Monad.Fix

import           Data.Maybe            (fromJust)
import           Data.Patch.Map

type LayerPos = Int

class (Ord (LayerEltId a)) => LayerElt a where
  type LayerEltId a :: Type
  isFolderStart :: a -> Bool
  isFolderEnd :: a -> Bool
  getId :: a -> LayerEltId a

type LayerView a = Seq a

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
  , _layerTree_changeView :: Event t (PatchMap (LayerEltId a) a) -- only elements that were added, moved, or deleted

  -- does this really need to be piped through LayerTree?
  --, _layerTree_copied     :: Dynamic t (LayerView a)
}

-- | use for inserting at end of list
layerTree_attachEndPos :: (Reflex t) => LayerTree t a -> Event t b -> Event t (LayerPos,b)
layerTree_attachEndPos LayerTree {..} = attach (length <$> current _layerTree_view)

-- | use for removing at end of list
layerTree_tagEndPos :: (Reflex t) => LayerTree t a -> Event t b -> Event t LayerPos
layerTree_tagEndPos LayerTree {..} = tag (length <$> current _layerTree_view)


-- | reindexes list of LayerPos such that each element is indexed as if all previous elements have been removed
-- O(n^2) lol
reindexLayerPosForRemoval :: [LayerPos] -> [LayerPos]
reindexLayerPosForRemoval [] = []
reindexLayerPosForRemoval (r:xs) = reindexLayerPosForRemoval rest where
  -- if this asserts that means you tried to remove the same index twice
  rest = map (\x -> assert (x /= r) $ if x > r then x-1 else x) xs

data LayerTreeConfig t a = LayerTreeConfig {
  -- | directory of elements
  _layerTreeConfig_directory :: Behavior t (Map (LayerEltId a) a)
  -- | ensure input 'LayerView' satsifies scoping property
  -- if 'LayerPos' is out of range, results in error
  , _layerTreeConfig_add     :: Event t (LayerPos, LayerView a)

  -- | ensure removing elements does not break scoping property
  -- throws error if the element does not exist
  , _layerTreeConfig_remove  :: Event t (NonEmpty LayerPos)

  -- | ensure copied elts satisfy scoping property
  --, _layerTreeConfig_copy    :: Event t (NonEmpty LayerPos)

  -- TODO this is a little weird with scoping
  -- ensure duplicated elts satisfy scoping property
  --, _layerTreeConfig_duplicate :: Event t [LayerEltId a]

  -- TODO make this work with many elements
  --, _layerTreeConfig_move      :: Event t (LayerPos, LayerPos) -- (from, to)
}

holdLayerTree :: forall t m a. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m, LayerElt a)
  => LayerTreeConfig t a
  -> m (LayerTree t a)
holdLayerTree LayerTreeConfig {..} = mdo
  let
    removeEv = fmap (\x -> (x,1)) removeSingleEv
    dseqc = DynamicSeqConfig {
        _dynamicSeqConfig_insert = _layerTreeConfig_add
        , _dynamicSeqConfig_remove = removeEv
        , _dynamicSeqConfig_clear = never
      }

  dseq :: DynamicSeq t a <-
    holdDynamicSeq empty dseqc
  (removeSingleEv, collectedRemovals) :: (Event t LayerPos, Event t [(Int, Seq a)]) <-
    repeatEventAndCollectOutput (reindexLayerPosForRemoval <$> toList <$> _layerTreeConfig_remove) (_dynamicSeq_removed dseq)

  let
    -- changes from collected removal
    changes1 :: Event t [(LayerEltId a, Maybe a)]
    changes1 = fmap join $ toList <$> (\(_, sa) -> (\a -> (getId a, Nothing)) <$> sa) <<$>> collectedRemovals
    -- changes from insertions
    changes2 :: Event t [(LayerEltId a, Maybe a)]
    changes2 = fmap toList $  (\a -> (getId a, Just a)) <<$>> snd <$> (_dynamicSeq_inserted dseq)

  return
    LayerTree {
      _layerTree_view = _dynamicSeq_contents dseq
      , _layerTree_changeView = PatchMap . fromList <$> (changes1 <> changes2)
    }
