{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.OwlWorkspace (
  OwlPFWorkspace(..)
  , emptyWorkspace
  , markWorkspaceSaved
  , undoWorkspace
  , redoWorkspace
  , undoPermanentWorkspace
  , doCmdWorkspace
  , WSEvent(..)
  , updateOwlPFWorkspace
  , loadOwlPFStateIntoWorkspace
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.OwlState
import           Potato.Flow.Types
import Potato.Flow.Llama
import Potato.Flow.Methods.Types

import           Control.Exception  (assert)
import           Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence      as Seq

-- TODO rename
data OwlPFWorkspace = OwlPFWorkspace {
  -- TODO rename owlPFState
  _owlPFWorkspace_pFState       :: OwlPFState

  -- this is updated after each call to updateOwlPFWorkspace and is only guaranteed to be valid at that point
  -- TODO better to have methods return (OwlPFWorkspace, SuperOwlChanges) instead of embedding in OwlPFWorkspace
  , _owlPFWorkspace_lastChanges :: SuperOwlChanges

  , _owlPFWorkspace_llamaStack :: LlamaStack
} deriving (Show, Generic)

instance NFData OwlPFWorkspace

loadOwlPFStateIntoWorkspace :: OwlPFState -> OwlPFWorkspace -> OwlPFWorkspace
loadOwlPFStateIntoWorkspace pfs ws = r where
  removeOld = fmap (const Nothing) (_owlTree_mapping . _owlPFState_owlTree . _owlPFWorkspace_pFState $ ws)
  addNew = IM.mapWithKey (\rid (oem,oe) -> Just (SuperOwl rid oem oe)) (_owlTree_mapping . _owlPFState_owlTree $ pfs)
  changes = IM.union addNew removeOld
  r = OwlPFWorkspace pfs changes emptyLlamaStack

emptyWorkspace :: OwlPFWorkspace
emptyWorkspace = OwlPFWorkspace emptyOwlPFState IM.empty emptyLlamaStack

-- UNTESTED
markWorkspaceSaved :: OwlPFWorkspace -> OwlPFWorkspace
markWorkspaceSaved pfw = r where
  as@LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  newas = as { _llamaStack_lastSaved = Just (length _llamaStack_done) }
  r = pfw { _owlPFWorkspace_llamaStack = newas }

undoWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
undoWorkspace pfw =  r where
  LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  r = case _llamaStack_done of
    c : cs -> OwlPFWorkspace newpfs changes (LlamaStack cs (undollama:_llamaStack_undone) _llamaStack_lastSaved) where
      (newpfs, changes, undollama) = case _llama_apply c (_owlPFWorkspace_pFState pfw) of
        Left e -> error $ show e
        Right x -> x
    _ -> pfw

redoWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
redoWorkspace pfw = r where
  LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  r = case _llamaStack_undone of
    c : cs -> OwlPFWorkspace newpfs changes (LlamaStack (dollama:_llamaStack_done) cs _llamaStack_lastSaved) where
      (newpfs, changes, dollama) = case _llama_apply c (_owlPFWorkspace_pFState pfw) of
        Left e -> error $ show e
        Right x -> x
    _ -> pfw

undoPermanentWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
undoPermanentWorkspace pfw =  r where
  LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  -- NOTE this step is rather unecessary as this is always followed by a doCmdWorkspace but it's best to keep the state correct in between in case anything changes in the future
  newLastSaved = case _llamaStack_lastSaved of
    Nothing -> Nothing
    Just x -> if length _llamaStack_done > x
      -- we are undoing a change that came after last save
      then Just x
      -- we are permanently undoing a change from last saved
      else Nothing
  r = case _llamaStack_done of
    c : cs -> OwlPFWorkspace newpfs changes (LlamaStack cs _llamaStack_undone newLastSaved) where
      (newpfs, changes, _) = case _llama_apply c (_owlPFWorkspace_pFState pfw) of
        Left e -> error $ show e
        Right x -> x
    _ -> pfw

doLlamaWorkspace :: Llama -> OwlPFWorkspace -> OwlPFWorkspace
doLlamaWorkspace llama pfw = r where
  (newpfs, changes, undollama) = case _llama_apply llama (_owlPFWorkspace_pFState pfw) of
    Left e -> error $ show e
    Right x -> x
  LlamaStack {..} = (_owlPFWorkspace_llamaStack pfw)
  newLastSaved = case _llamaStack_lastSaved of
    Nothing -> Nothing
    Just x -> if length _llamaStack_done < x
      -- we "did" something when last save is still on undo stack, so we can never recover to last saved
      then Nothing
      -- we can still undo back to last save state
      else Just x
  r = OwlPFWorkspace {
      _owlPFWorkspace_pFState       = newpfs
      , _owlPFWorkspace_lastChanges = changes
      , _owlPFWorkspace_llamaStack  = LlamaStack {
          _llamaStack_done = undollama : _llamaStack_done
          , _llamaStack_undone = _llamaStack_undone
          , _llamaStack_lastSaved = newLastSaved
        }
    }

doLlamaWorkspaceUndoPermanentFirst :: Llama -> OwlPFWorkspace -> OwlPFWorkspace
doLlamaWorkspaceUndoPermanentFirst llama ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  undoedws = undoPermanentWorkspace ws
  r = doLlamaWorkspace llama undoedws

doCmdWorkspace :: OwlPFCmd -> OwlPFWorkspace -> OwlPFWorkspace
doCmdWorkspace cmd pfw = force r where
  r = doLlamaWorkspace (makePFCLlama cmd) pfw

doCmdOwlPFWorkspaceUndoPermanentFirst :: (OwlPFState -> OwlPFCmd) -> OwlPFWorkspace -> OwlPFWorkspace
doCmdOwlPFWorkspaceUndoPermanentFirst cmdFn ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  undoedws = undoPermanentWorkspace ws
  undoedpfs = _owlPFWorkspace_pFState undoedws
  cmd = cmdFn undoedpfs
  r = doLlamaWorkspace (makePFCLlama cmd) undoedws

------ update functions via commands
data WSEvent =
  WSEAddElt (Bool, OwlSpot, OwlItem)

  -- TODO CAN DELETE has been replaced by WSEAddTree
  -- TODO won't work, needs to support OwlItems with kiddos, need MiniOwlTree
  -- it's a little weird that MiniOwlTree is already reindexed though...
  -- maybe just take a selttree D:
  -- I can't remember why I called this WSEAddRelative D:
  | WSEAddRelative (OwlSpot, Seq OwlItem)

  | WSEAddTree (OwlSpot, MiniOwlTree)

  | WSEAddFolder (OwlSpot, Text)
  | WSERemoveElt OwlParliament
  | WSEMoveElt (OwlSpot, OwlParliament)
  -- | WSEDuplicate OwlParliament -- kiddos get duplicated??

  -- DEPRECATE
  | WSEManipulate (Bool, ControllersWithId)

  | WSEApplyLlama (Bool, Llama)

  | WSEResizeCanvas DeltaLBox
  | WSEUndo
  | WSERedo
  | WSELoad SPotatoFlow
  deriving (Show)

debugPrintBeforeAfterState :: (IsString a) => OwlPFState -> OwlPFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintOwlPFState stateBefore <> "\nAFTER: " <> debugPrintOwlPFState stateAfter


------ helpers for converting events to cmds
-- TODO assert elts are valid
pfc_addElt_to_newElts :: OwlPFState -> OwlSpot -> OwlItem -> OwlPFCmd
pfc_addElt_to_newElts pfs spot oelt = OwlPFCNewElts [(owlPFState_nextId pfs, spot, oelt)]

-- TODO assert elts are valid
pfc_addRelative_to_newElts :: OwlPFState -> (OwlSpot, Seq OwlItem) -> OwlPFCmd
pfc_addRelative_to_newElts pfs (ospot, oelts) = r where
  startid = owlPFState_nextId pfs
  mapAccumLFn (i,ospotacc) oelt = ((i+1, nextacc), (rid, ospotacc, oelt)) where
    -- order from left to right so there is valid leftSibling
    nextacc = ospotacc { _owlSpot_leftSibling = Just rid}
    rid = startid + i
  (_, r') = mapAccumL mapAccumLFn (0,ospot) oelts
  r = OwlPFCNewElts $ toList r'

--TODO need to reorder so it becomes undo friendly here I think? (uhh, pretty sure it's ok to delete this TODO? should be ordered by assumption)
-- TODO assert elts are valid
pfc_moveElt_to_move :: OwlPFState -> (OwlSpot, OwlParliament) -> OwlPFCmd
pfc_moveElt_to_move pfs (ospot, op) = OwlPFCMove (ospot, owlParliament_toSuperOwlParliament (_owlPFState_owlTree pfs) op)

pfc_removeElt_to_deleteElts :: OwlPFState -> OwlParliament -> OwlPFCmd
pfc_removeElt_to_deleteElts pfs owlp = assert valid r where
  od = _owlPFState_owlTree pfs
  valid = superOwlParliament_isValid od $ owlParliament_toSuperOwlParliament od owlp
  sop = owlParliament_toSuperOwlParliament od owlp
  sowlswithchildren = superOwlParliament_convertToSeqWithChildren od sop
  r = OwlPFCDeleteElts $ toList (fmap (\SuperOwl {..} -> (_superOwl_id, owlTree_owlItemMeta_toOwlSpot od _superOwl_meta, _superOwl_elt)) sowlswithchildren)

pfc_addFolder_to_newElts :: OwlPFState -> (OwlSpot, Text) -> OwlPFCmd
pfc_addFolder_to_newElts pfs (spot, name) = OwlPFCNewElts [(owlPFState_nextId pfs, spot, OwlItem (OwlInfo name) (OwlSubItemFolder Seq.empty))]

-- | takes a DeltaLBox transformation and clamps it such that it always produces a canonical box
-- if starting box was non-canonical, this will create a DeltaLBox that forces it to be canonical
-- assumes input box is canonical
clampNoNegDeltaLBoxTransformation :: LBox -> DeltaLBox -> DeltaLBox
clampNoNegDeltaLBoxTransformation lbx@(LBox (V2 x y) (V2 w h)) (DeltaLBox (V2 dx dy) (V2 dw dh)) = assert (lBox_isCanonicalLBox lbx) r where

  -- var legend
  -- ogt = original translation
  -- ogd = original dimension
  -- dt = delta translation
  -- dd = delta dimension

  clamptfirst ogt ogd dt dd = (nt, nd) where
    nt = min (ogt+dt) (ogt + max ogd (ogd+dd))
    nd = max 0 (ogd+dd)

  clampdfirst ogt ogd dt dd = (nt, nd) where
    nd = max 0 (ogd+dd)
    nt = min (ogt+dt) (ogt+nd)

  computepair ogt ogd dt dd = if ogd + dd < 0
    -- if there was a flip
    then if dt /= 0
      -- if we're moving from the translation side then clamp the translation first
      then clamptfirst ogt ogd dt dd
      else clampdfirst ogt ogd dt dd
    -- else plusDelta on components as normal
    else (ogt+dt, ogd+dd)

  (nx, nw) = computepair x w dx dw
  (ny, nh) = computepair y h dy dh

  r = DeltaLBox (V2 (nx-x) (ny-y)) (V2 (nw-w) (nh-h))

pfc_manipulate_to_manipulate :: OwlPFState -> ControllersWithId -> OwlPFCmd
pfc_manipulate_to_manipulate pfs cwid = r where

  -- TODO probably move somwhere else
  -- restrict boxes to prevent negative
  mapfn rid controller = case controller of
    (CTagBoundingBox :=> Identity x) -> newbb where
      db' = _cBoundingBox_deltaBox x
      sowl = owlTree_mustFindSuperOwl (_owlPFState_owlTree pfs) rid
      msbox = getSEltBox_naive $ superOwl_toSElt_hack sowl
      db = case msbox of
        Nothing -> db'
        Just sbox -> clampNoNegDeltaLBoxTransformation sbox db'
      newbb = CTagBoundingBox :=> Identity (CBoundingBox db)
    x -> x

  r = OwlPFCManipulate $ IM.mapWithKey mapfn cwid


updateOwlPFWorkspace :: WSEvent -> OwlPFWorkspace -> OwlPFWorkspace
updateOwlPFWorkspace evt ws = let
  lastState = _owlPFWorkspace_pFState ws
  r = case evt of
    WSEAddElt (undo, spot, oelt) -> if undo
      then doCmdOwlPFWorkspaceUndoPermanentFirst (\pfs -> pfc_addElt_to_newElts pfs spot oelt) ws
      else doCmdWorkspace (pfc_addElt_to_newElts lastState spot oelt) ws
    WSEAddRelative x -> doCmdWorkspace (pfc_addRelative_to_newElts lastState x) ws
    WSEAddTree x -> doCmdWorkspace (OwlPFCNewTree (swap x)) ws
    WSEAddFolder x -> doCmdWorkspace (pfc_addFolder_to_newElts lastState x) ws
    WSERemoveElt x -> doCmdWorkspace (pfc_removeElt_to_deleteElts lastState x) ws

    -- DELETE
    WSEManipulate (undo, x) -> if undo
      then doCmdOwlPFWorkspaceUndoPermanentFirst (\pfs -> pfc_manipulate_to_manipulate pfs x) ws
      else doCmdWorkspace (pfc_manipulate_to_manipulate lastState x) ws

    -- TODO
    WSEApplyLlama (undo, x) -> if undo
      then doLlamaWorkspaceUndoPermanentFirst x ws
      else doLlamaWorkspace x ws


    WSEMoveElt x -> doCmdWorkspace (pfc_moveElt_to_move lastState x) ws
    WSEResizeCanvas x -> doCmdWorkspace (OwlPFCResizeCanvas x) ws
    WSEUndo -> undoWorkspace ws
    WSERedo -> redoWorkspace ws
    WSELoad x -> loadOwlPFStateIntoWorkspace (sPotatoFlow_to_owlPFState x) ws
  afterState = _owlPFWorkspace_pFState r
  isValidAfter = owlPFState_isValid afterState
  in if isValidAfter
    --then trace "WORKSPACE UPDATE" $ traceShow evt $ trace (T.unpack $ potatoShow (_owlPFState_owlTree afterState)) $ r
    then r
    else error ("INVALID " <> show evt <> "\n" <> debugPrintBeforeAfterState lastState afterState)
