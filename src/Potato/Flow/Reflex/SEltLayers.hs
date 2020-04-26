{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE RecursiveDo             #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Potato.Flow.Reflex.SEltLayers (
  LayerPos
  , SEltLayerTree(..)
  , sEltLayerTree_sampleSuperSEltByPos
  , sEltLayerTree_tagSuperSEltByPos
  , SEltLayerTreeConfig(..)
  , holdSEltLayerTree
) where

import           Relude

import           Reflex
import           Reflex.Data.Directory
import           Reflex.Data.Sequence
import           Reflex.Potato.Helpers

import           Potato.Flow.Reflex.RElts
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Exception        (assert)
import           Control.Monad.Fix

import qualified Data.IntMap.Strict       as IM
import qualified Data.List                as L
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe               (fromJust)
import qualified Data.Sequence            as Seq



--isFolderStart :: a -> Bool
--isFolderEnd :: a -> Bool


-- TODO needs map
-- | checks if 'SEltLayerView' satisfies scoping property
{-
isValidSEltLayerView :: (SEltLayerElt a) => SEltLayerView a -> Bool
isValidSEltLayerView lv = foldl' foldfn 0 lv == 0 where
  foldfn scope elt = (+) scope $ if isFolderStart elt
    then 1
    else if isFolderEnd elt then (-1)
    else 0
-}

-- | layer tree does not provide an interface to ensure scoping property is satisfied
-- if inputs are correct, than outputs are correct, so corroctly use correct outputs to ensure inputs are always correct!
data SEltLayerTree t = SEltLayerTree {
  _sEltLayerTree_view         :: Dynamic t (Seq REltId)

  --, _sEltLayerTree_copied     :: Dynamic t (Seq SEltLabel)
  , _sEltLayerTree_changeView :: Event t (REltIdMap (Maybe SEltLabel, Maybe SEltLabel)) -- elements that were added, moved, or deleted

  -- | directory of all SEltLabels
  , _sEltLayerTree_directory  :: Directory t (SEltLabel)
}


-- TODO do I still need these?
{-
-- | use for inserting at end of list
sEltLayerTree_attachEndPos :: (Reflex t) => SEltLayerTree t -> Event t b -> Event t (LayerPos, b)
sEltLayerTree_attachEndPos SEltLayerTree {..} = attach (length <$> current _sEltLayerTree_view)
-- | use for removing at end of list
sEltLayerTree_tagEndPos :: (Reflex t) => SEltLayerTree t -> Event t b -> Event t LayerPos
sEltLayerTree_tagEndPos SEltLayerTree {..} = tag (length <$> current _sEltLayerTree_view)
-}

sEltLayerTree_sampleSuperSEltByPos :: forall t. (Reflex t) => SEltLayerTree t -> LayerPos -> PushM t (Maybe SuperSEltLabel)
sEltLayerTree_sampleSuperSEltByPos SEltLayerTree {..} lp = do
  layers <- sample . current $ _sEltLayerTree_view
  let rid = fromJust $ Seq.lookup lp layers
  slmap <- sample $ _directory_contents _sEltLayerTree_directory
  let msl = IM.lookup rid slmap
  return $ do
    sl <- msl
    return (rid, lp, sl)

-- | tag the SElt at the input position
sEltLayerTree_tagSuperSEltByPos :: forall t. (Reflex t) => SEltLayerTree t -> Event t LayerPos -> Event t (SuperSEltLabel)
sEltLayerTree_tagSuperSEltByPos slt = push $ sEltLayerTree_sampleSuperSEltByPos slt

-- TODO
-- | tag SEltLabels at the input position
--sEltLayerTree_tagSuperSEltByPos :: forall t. (Reflex t) => SEltLayerTree t -> Event t [LayerPos] ->  Event t [SuperSEltLabel]


{-
-- DELETE
-- | tag the SElt at the input REltId
sEltLayerTree_tagSEltById :: (Reflex t) => SEltLayerTree t -> Event t REltId -> Event t (REltId, SEltLabel)
sEltLayerTree_tagSEltById SEltLayerTree {..} = pushAlways $ \rid -> do
  slmap <- sample $ _directory_contents _sEltLayerTree_directory
  let msl = IM.lookup rid slmap
  return (rid, fromJust msl) -- PARTIAL
-}

-- | reindexes list of LayerPos such that each element is indexed as if all previous elements have been removed
-- O(n^2) lol
reindexSEltLayerPosForRemoval :: [LayerPos] -> [LayerPos]
reindexSEltLayerPosForRemoval [] = []
reindexSEltLayerPosForRemoval (r:xs) = r:reindexSEltLayerPosForRemoval rest where
  -- if this asserts that means you tried to remove the same index twice
  rest = map (\x -> assert (x /= r) $ if x > r then x-1 else x) xs

-- | inverse of reindexSEltLayerPosForRemoval
reindexSEltLayerPosForInsertion :: [LayerPos] -> [LayerPos]
reindexSEltLayerPosForInsertion = reverse . reindexSEltLayerPosForRemoval . reverse

data SEltLayerTreeConfig t = SEltLayerTreeConfig {
  -- error if any REltId or LayerPos are invalid
  -- LayerPos indices are as if all elements already exist in the map
  _sEltLayerTreeConfig_insert               :: Event t (NonEmpty SuperSEltLabel)
  -- error if any REltId or LayerPos are invalid
  -- LayerPos indices are the current indices of elements to be removed
  -- this contains more info than needed to remove, but we need to track it anyways for undoing removals so this just makes life easier
  , _sEltLayerTreeConfig_remove             :: Event t (NonEmpty SuperSEltLabel)

  -- TODO 0
  --, _sEltLayerTreeConfig_modify :: Event t ControllersWithId

  -- TODO
  --, _sEltLayerTreeConfig_copy
  --, _sEltLayerTreeConfig_duplicate
  --, _sEltLayerTreeConfig_move


  -- | pass through modifiers for SEltLabels in directory
  , _sEltLayerTree_directory_doManipulate   :: Event t ControllersWithId
  , _sEltLayerTree_directory_undoManipulate :: Event t ControllersWithId
}

holdSEltLayerTree :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => SEltLayerTreeConfig t
  -> m (SEltLayerTree t)
holdSEltLayerTree SEltLayerTreeConfig {..} = mdo

  let
    removeLayerPos :: SuperSEltLabel -> (REltId, SEltLabel)
    removeLayerPos (rid,_,e) = (rid,e)
    extractREltId :: SuperSEltLabel -> REltId
    extractREltId (rid,_,_) = rid
    flattenControls :: Bool -> IntMap Controller -> [(REltId, SEltLabel -> SEltLabel)]
    flattenControls isDo im = IM.toList $ flip IM.map im $ updateFnFromController isDo
    -- PARTIAL
    modifyDo :: Event t (NonEmpty (REltId, SEltLabel -> SEltLabel))
    modifyDo = fmap NE.fromList $ ffor _sEltLayerTree_directory_doManipulate $ flattenControls True
    -- PARTIAL
    modifyUndo :: Event t (NonEmpty (REltId, SEltLabel -> SEltLabel))
    modifyUndo = fmap NE.fromList $ ffor _sEltLayerTree_directory_undoManipulate $ flattenControls False

    directoryConfig = DirectoryConfig {
        _directoryConfig_add = removeLayerPos <<$>> _sEltLayerTreeConfig_insert
        , _directoryConfig_remove = extractREltId <<$>> _sEltLayerTreeConfig_remove
        , _directoryConfig_modifyWith = leftmostwarn "directory modify" [modifyDo, modifyUndo]
      }
  directory :: Directory t SEltLabel
    <- holdDirectory directoryConfig

  -- TODO add native support for adding many elts at once to seq so you don't have to do this
  -- step insert and remove events
  let
    extractLayerPos :: SuperSEltLabel -> LayerPos
    extractLayerPos (_,p,_) = p
    inputRemoveEv :: Event t [(LayerPos, Int)]
    inputRemoveEv = fmap (\x -> (x,1))
      <$> reindexSEltLayerPosForRemoval
      <$> toList
      <$> fmap extractLayerPos
      <$> _sEltLayerTreeConfig_remove
    prepForInsertion :: SuperSEltLabel -> (LayerPos, Seq REltId)
    prepForInsertion (rid, lp, _) = (lp, Seq.singleton rid)
    inputInsertEv :: Event t [(LayerPos, Seq REltId)]
    inputInsertEv = toList <$> fmap prepForInsertion <$> _sEltLayerTreeConfig_insert
  (removeSingleEv, collectedRemovals) :: (Event t (LayerPos, Int), Event t [NonEmpty (REltId, SEltLabel)]) <-
    repeatEventAndCollectOutput inputRemoveEv (_directory_removed directory)
  (insertSingleEv, collectedInsertions) :: (Event t (LayerPos, Seq REltId), Event t [NonEmpty (REltId, SEltLabel)]) <-
    repeatEventAndCollectOutput inputInsertEv (_directory_added directory)

  let
    dseqc = DynamicSeqConfig {
        _dynamicSeqConfig_insert = insertSingleEv
        , _dynamicSeqConfig_remove = removeSingleEv
        , _dynamicSeqConfig_clear = never
      }
  dseq :: DynamicSeq t REltId <-
    holdDynamicSeq empty dseqc

  let

    -- TODO THIS LEAKS FIX ME
    -- changes from removal
    changes1 :: Event t [(REltId, (Maybe SEltLabel, Maybe SEltLabel))]
    changes1 = NE.toList
      <$> fmap (\(rid, seltl) -> (rid, (Just seltl, Nothing)))
      <$> L.head -- PARTIAL
      <$> collectedRemovals

    -- TODO THIS LEAKS FIX ME
    -- changes from insertions
    changes2 :: Event t [(REltId, (Maybe SEltLabel, Maybe SEltLabel))]
    changes2 = NE.toList
      <$> fmap (\(rid, seltl) -> (rid, (Nothing, Just seltl)))
      <$> L.head -- PARTIAL
      <$> collectedInsertions

    -- changes from modifications
    changes3 :: Event t [(REltId, (Maybe SEltLabel, Maybe SEltLabel))]
    changes3 = NE.toList
      <$> fmap (\(rid, before, after) -> (rid, (Just before, Just after)))
      <$> _directory_modified directory

  return
    SEltLayerTree {
      _sEltLayerTree_view = _dynamicSeq_contents dseq
      , _sEltLayerTree_directory = directory
      -- TODO FIX, disabled to prevent know leaks (so I can find other ones easily lol)
      , _sEltLayerTree_changeView = never --IM.fromList <$> leftmostwarn "SEltLayerTree changes" [changes1, changes2, changes3]
    }
