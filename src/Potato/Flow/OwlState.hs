{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.OwlState where

import           Relude

import Potato.Flow.Owl
import           Potato.Flow.Deprecated.Layers
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception       (assert)
import           Data.Aeson
import qualified Data.IntMap.Strict      as IM
import           Data.List.Ordered       (isSortedBy)
import           Data.Maybe
import qualified Data.Sequence           as Seq
import qualified Data.Text as T


-- TODO rename
data OwlPFState = OwlPFState {
  _owlPFState_owlTree :: OwlTree
  , _owlPFState_canvas    :: SCanvas
} deriving (Show, Eq, Generic)

instance NFData OwlPFState

owlPFState_nextId :: OwlPFState -> REltId
owlPFState_nextId pfs = (+1) . owlTree_maxId . _owlPFState_owlTree $ pfs

owlPFState_lastId :: OwlPFState -> REltId
owlPFState_lastId pfs = owlTree_maxId . _owlPFState_owlTree $ pfs

owlPFState_numElts :: OwlPFState -> Int
owlPFState_numElts pfs = IM.size . _owlTree_mapping . _owlPFState_owlTree $ pfs

-- TODO DELETE replace with owlTree_prettyPrint
debugPrintOwlPFState :: (IsString a) => OwlPFState -> a
debugPrintOwlPFState OwlPFState {..} = fromString . T.unpack $ owlTree_prettyPrint _owlPFState_owlTree

-- TODO owlPFState_selectionIsValid pfs OwlParliament $ Seq.fromList [0..Seq.length _owlPFState_layers - 1]
owlPFState_isValid :: OwlPFState -> Bool
owlPFState_isValid pfs@OwlPFState {..} = True

owlPFState_selectionIsValid :: OwlPFState -> OwlParliament -> Bool
owlPFState_selectionIsValid OwlPFState {..} (OwlParliament op) = validElts where
  OwlTree {..} = _owlPFState_owlTree
  validElts = all isJust . toList $ fmap ((IM.!?) _owlTree_mapping) op

-- TODO replace with superOwlParliament_toSEltTree
owlPFState_copyElts :: OwlPFState -> OwlParliament -> [SEltLabel]
owlPFState_copyElts OwlPFState {..} op = r where
  sop = owlParliament_toSuperOwlParliament _owlPFState_owlTree op
  r = fmap snd $ superOwlParliament_toSEltTree _owlPFState_owlTree sop

-- TODO replace with owlTree_findSuperOwl
owlPFState_getSuperOwls :: OwlPFState -> [REltId] -> REltIdMap (Maybe SuperOwl)
owlPFState_getSuperOwls OwlPFState {..} rids = foldr (\rid acc -> IM.insert rid (owlTree_findSuperOwl rid _owlPFState_owlTree) acc) IM.empty rids

emptyPFState :: OwlPFState
emptyPFState = OwlPFState emptyOwlTree (SCanvas (LBox 0 0))

sPotatoFlow_to_owlPFState :: SPotatoFlow -> OwlPFState
sPotatoFlow_to_owlPFState SPotatoFlow {..} = r where
  r = OwlPFState (owlTree_fromSEltTree _sPotatoFlow_sEltTree) _sPotatoFlow_sCanvas

owlPFState_to_sPotatoFlow :: OwlPFState -> SPotatoFlow
owlPFState_to_sPotatoFlow OwlPFState {..} = r where
  selttree = owlTree_toSEltTree _owlPFState_owlTree
  r = SPotatoFlow _owlPFState_canvas selttree

owlPFState_toCanvasCoordinates :: OwlPFState -> XY -> XY
owlPFState_toCanvasCoordinates OwlPFState {..} (V2 x y) = V2 (x-sx) (y-sy) where
  LBox (V2 sx sy) _ = _sCanvas_box _owlPFState_canvas

owlPFState_to_SuperOwlParliament :: OwlPFState -> SuperOwlParliament
owlPFState_to_SuperOwlParliament OwlPFState {..} = owlParliament_toSuperOwlParliament _owlPFState_owlTree $ OwlParliament $ _owlTree_topOwls _owlPFState_owlTree


-- UNTESTED
do_newElts :: [(REltId, OwlSpot, OwlElt)] -> OwlPFState -> (OwlPFState, SuperOwlChanges)
do_newElts seltls pfs@OwlPFState {..} = r where
  mapaccumlfn od (rid,ospot,oelt) = owlTree_addOwlElt ospot rid oelt od
  (newot, changes') = mapAccumL mapaccumlfn _owlPFState_owlTree seltls
  changes = IM.fromList $ fmap (\sowl -> (_superOwl_id sowl, Just sowl)) changes'
  r = (pfs { _owlPFState_owlTree = newot}, changes)

-- UNTESTED
undo_newElts :: [(REltId, OwlSpot, OwlElt)] -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undo_newElts seltls pfs@OwlPFState {..} = r where
  foldfn (rid,_,_) od = owlTree_removeREltId rid od
  newot = foldr foldfn _owlPFState_owlTree seltls
  changes = IM.fromList $ fmap (\(rid,_,_) -> (rid, Nothing)) seltls
  r = (pfs { _owlPFState_owlTree = newot}, changes)

do_deleteElts :: [(REltId, OwlSpot, OwlElt)] -> OwlPFState -> (OwlPFState, SuperOwlChanges)
do_deleteElts = undo_newElts

undo_deleteElts :: [(REltId, OwlSpot, OwlElt)] -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undo_deleteElts = do_newElts

do_move :: (OwlSpot, SuperOwlParliament) -> OwlPFState -> (OwlPFState, SuperOwlChanges)
do_move (os, sop) pfs@OwlPFState {..} = assert isUndoFriendly r where

  -- make sure SuperOwlParliament is ordered in an undo-friendly way
  rp = _owlEltMeta_position . _superOwl_meta
  isUndoFriendly = isSortedBy (\sowl1 sowl2 -> (rp sowl1) < (rp sowl2)) . toList . unSuperOwlParliament $ sop

  op = superOwlParliament_toOwlParliament sop
  (newot, changes') = owlTree_moveOwlParliament op os _owlPFState_owlTree
  changes = IM.fromList $ fmap (\sowl -> (_superOwl_id sowl, Just sowl)) changes'
  r = (pfs { _owlPFState_owlTree = newot}, changes)

undo_move :: (OwlSpot, SuperOwlParliament) -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undo_move (os, sop) pfs@OwlPFState {..} = assert isUndoFriendly r where

  -- NOTE that sop is likely invalid in pfs at this point

  -- make sure SuperOwlParliament is ordered in an undo-friendly way
  rp = _owlEltMeta_position . _superOwl_meta
  isUndoFriendly = isSortedBy (\sowl1 sowl2 -> (rp sowl1) < (rp sowl2)) . toList . unSuperOwlParliament $ sop

  -- first remove all elements we moved
  removefoldfn tree' so = owlTree_removeREltId (_superOwl_id so) tree'
  removedTree = foldl' removefoldfn _owlPFState_owlTree (unSuperOwlParliament sop)

  -- then add them back in in order
  addmapaccumlfn tree' so = owlTree_addOwlElt ospot (_superOwl_id so) (_superOwl_elt so) tree' where
    -- NOTE that because we are ordered from left to right, _superOwl_meta so is valid in tree'
    ospot = owlTree_owlEltMeta_toOwlSpot tree' $ _superOwl_meta so
  (addedTree, changes') = mapAccumL addmapaccumlfn removedTree (unSuperOwlParliament sop)

  changes = IM.fromList $ fmap (\sowl -> (_superOwl_id sowl, Just sowl)) (toList changes')
  r = (pfs { _owlPFState_owlTree = addedTree}, changes)


-- OwlElt compatible variant of updateFnFromController
updateFnFromControllerOwl :: Bool -> Controller -> ((OwlEltMeta, OwlElt)->(OwlEltMeta, OwlElt))
updateFnFromControllerOwl isDo controller = r where
  f = updateFnFromController isDo controller
  -- 😱😱😱
  rewrap oem mkiddos (SEltLabel name elt) = case elt of
    SEltFolderStart -> (oem, OwlEltFolder (OwlInfo name) (fromJust mkiddos))
    s -> (oem, OwlEltSElt (OwlInfo name) s)
  r (oem, oe) = case oe of
    OwlEltFolder oi kiddos -> rewrap oem (Just kiddos) $ f (SEltLabel (_owlInfo_name oi) SEltFolderStart)
    OwlEltSElt oi selt -> rewrap oem Nothing $ f (SEltLabel (_owlInfo_name oi) selt)

manipulate :: Bool -> ControllersWithId -> OwlPFState -> (OwlPFState, SuperOwlChanges)
manipulate isDo cs pfs = (r, fmap Just changes) where
  mapping = _owlTree_mapping . _owlPFState_owlTree $ pfs
  changes' = IM.intersectionWith (updateFnFromControllerOwl isDo) cs mapping
  newMapping = IM.union changes' mapping
  changes = IM.mapWithKey (\k (oem, oe) -> SuperOwl k oem oe) changes'
  r = pfs { _owlPFState_owlTree = (_owlPFState_owlTree pfs) { _owlTree_mapping = newMapping } }

do_manipulate :: ControllersWithId -> OwlPFState -> (OwlPFState, SuperOwlChanges)
do_manipulate = manipulate True

undo_manipulate :: ControllersWithId -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undo_manipulate = manipulate False

-- | check if the SCanvas is valid or not
-- for now, canvas offset must always be 0, I forget why it's even an option to offset the SCanvas, probably potatoes.
isValidCanvas :: SCanvas -> Bool
isValidCanvas (SCanvas (LBox p (V2 w h))) = p == 0 && w > 0 && h > 0

do_resizeCanvas :: DeltaLBox -> OwlPFState -> OwlPFState
do_resizeCanvas d pfs = assert (isValidCanvas newCanvas) $ pfs { _owlPFState_canvas = newCanvas } where
  newCanvas = SCanvas $ plusDelta (_sCanvas_box (_owlPFState_canvas pfs)) d

undo_resizeCanvas :: DeltaLBox -> OwlPFState -> OwlPFState
undo_resizeCanvas d pfs = assert (isValidCanvas newCanvas) $ pfs { _owlPFState_canvas = newCanvas } where
  newCanvas = SCanvas $ minusDelta (_sCanvas_box (_owlPFState_canvas pfs)) d
