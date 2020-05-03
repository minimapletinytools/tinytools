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
  slmap <- sample . current $ _directory_contents _sEltLayerTree_directory
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

  -- | error if any REltId or LayerPos are invalid
  -- LayerPos indices are as if all elements already exist in the map
  -- MANY FRAMES
  _sEltLayerTreeConfig_insert               :: Event t (NonEmpty SuperSEltLabel)

  -- | error if any REltId or LayerPos are invalid
  -- LayerPos indices are the current indices of elements to be removed
  -- this contains more info than needed to remove, but we need to track it anyways for undoing removals so this just makes life easier
  -- MANY FRAMES
  , _sEltLayerTreeConfig_remove             :: Event t (NonEmpty SuperSEltLabel)

  -- TODO
  --, _sEltLayerTreeConfig_copy
  --, _sEltLayerTreeConfig_duplicate

  -- | first argument is position of elements BEFORE removal, second argument is elements to move
  -- TODO this is no good, need to be able to undo
  --, _sEltLayerTreeConfig_move :: Event t (LayerPos, NonEmpty LayerPos)


  -- | pass through modifiers for SEltLabels in directory
  , _sEltLayerTree_directory_doManipulate   :: Event t ControllersWithId
  , _sEltLayerTree_directory_undoManipulate :: Event t ControllersWithId

  -- | load a new SEltLayertree, elements are assumed to be in order
  , _sEltLayerTreeConfig_load               :: Event t [(REltId, SEltLabel)]
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
        -- TODO assert that insert/removal events satisfy scoping property
        _directoryConfig_add = removeLayerPos <<$>> _sEltLayerTreeConfig_insert
        , _directoryConfig_remove = extractREltId <<$>> _sEltLayerTreeConfig_remove
        , _directoryConfig_modifyWith = leftmostwarn "directory modify" [modifyDo, modifyUndo]
        , _directoryConfig_set = _sEltLayerTreeConfig_load
      }
  directory :: Directory t SEltLabel
    <- holdDirectory directoryConfig


  -- insert and remove events
  ---------------------------
  -- TODO add native support for adding many elts at once to seq so you don't have to do this
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
    insertAssertPred :: Maybe LayerPos -> (LayerPos, Seq REltId) -> Maybe LayerPos
    insertAssertPred mlp (lp,_) = do
      plp <- mlp
      guard $ plp < lp
      return lp
    inputInsertEv :: Event t [(LayerPos, Seq REltId)]
    inputInsertEv = assertEvent "SEltLayers insert out of order" (isJust . foldl' insertAssertPred (Just (-1)))
      $ toList
      <$> fmap prepForInsertion
      <$> _sEltLayerTreeConfig_insert
  (removeSingleEv, collectedRemovals) :: (Event t (LayerPos, Int), Event t [NonEmpty (REltId, SEltLabel)]) <-
    stepEventsAndCollectOutput inputRemoveEv (_directory_removed directory)
  (insertSingleEv, collectedInsertions) :: (Event t (LayerPos, Seq REltId), Event t [NonEmpty (REltId, SEltLabel)]) <-
    stepEventsAndCollectOutput inputInsertEv (_directory_added directory)

  -- load events
  --------------
  let
    prepManyForInsertion :: [(REltId, SEltLabel)] -> (LayerPos, Seq REltId)
    prepManyForInsertion seltls = (0, Seq.fromList $ fmap fst seltls)
  -- clear first then load
  -- _sEltLayerTreeConfig_load is connected to _dynamicSeqConfig_clear and _directoryConfig_set
  -- insertLoadEv is connected to _dynamicSeqConfig_insert
  insertLoadEv <- sequenceEvents _sEltLayerTreeConfig_load
    $ fmap prepManyForInsertion _sEltLayerTreeConfig_load

  -- move events
  --------------
  {- TODO This is no good, because you need to be able to undo move operations
  -- TODO assert that move event contents satisfies scoping property
  -- store insertion position when we start a move event
  moveInsertPos <- holdDyn Nothing $ fmap (Just . fst) _sEltLayerTreeConfig_move
  -- remove stuff first
  (moveRemoveSingleEv, moveRemoveDone) :: (Event t (LayerPos, Int), Event t [(Int, Seq a)]) <-
    flip stepEventsAndSequenceCollectOutput (_dynamicSeq_removed dseq)
    $ fmap (fmap (\x -> (x,1)))
    $ fmap reindexSEltLayerPosForRemoval
    $ fmap NE.toList
    $ fmap snd
    $ _sEltLayerTreeConfig_move
  -- add it back in all in one go
  -- TODO can we get rid of fromJust/attachPromptlyDyn here somehow?
  let
    moveInsertEv :: Event t (LayerPos, Seq REltId)
    moveInsertEv = attach (current $ fmap fromJust moveInsertPos) $ fmap (mconcat . fmap snd) moveRemoveDone
  -}


  -- REltId -> Index map
  ----------------------
  -- TODO
  -- listen to insertions, removals, and moves to reconstruct map from first index that changed


  -- create DynamicSeq
  ------------------------------------------------
  let
    dseqc = DynamicSeqConfig {
        _dynamicSeqConfig_insert = leftmostwarn "layer seq insert" [insertSingleEv, insertLoadEv]
        , _dynamicSeqConfig_remove = leftmostwarn "layer seq remove" [removeSingleEv]
        , _dynamicSeqConfig_clear = void _sEltLayerTreeConfig_load
      }
  dseq :: DynamicSeq t REltId <-
    holdDynamicSeq empty dseqc

  -- collect changes for _sEltLayerTree_changeView
  ------------------------------------------------
  let
    -- 'force' is needed to prevent leaks D:
    -- changes from removal
    changes1 :: Event t [(REltId, (Maybe SEltLabel, Maybe SEltLabel))]
    changes1 = force <$> NE.toList
      <$> fmap (\(rid, seltl) -> (rid, (Just seltl, Nothing)))
      <$> L.head -- PARTIAL
      <$> collectedRemovals

    -- changes from insertions
    changes2 :: Event t [(REltId, (Maybe SEltLabel, Maybe SEltLabel))]
    changes2 =  force <$> NE.toList
      <$> fmap (\(rid, seltl) -> (rid, (Nothing, Just seltl)))
      <$> L.head -- PARTIAL
      <$> collectedInsertions

    -- changes from modifications
    changes3 :: Event t [(REltId, (Maybe SEltLabel, Maybe SEltLabel))]
    changes3 = force <$> NE.toList
      <$> fmap (\(rid, before, after) -> (rid, (Just before, Just after)))
      <$> _directory_modified directory

  return
    SEltLayerTree {
      _sEltLayerTree_view = _dynamicSeq_contents dseq
      , _sEltLayerTree_directory = directory
      , _sEltLayerTree_changeView = IM.fromList <$> leftmostwarn "SEltLayerTree changes" [changes1, changes2, changes3]
    }
