{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.OwlWorkspace (
  OwlPFWorkspace(..)
  , emptyWorkspace
  , emptyActionStack
  , markWorkspaceSaved
  , actionStack_hasUnsavedChanges
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
import           Potato.Flow.SElts
import Potato.Flow.SEltMethods
import           Potato.Flow.Owl
import           Potato.Flow.OwlState
import           Potato.Flow.Types
import Potato.Flow.Methods.Types

import           Control.Exception  (assert)
import           Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence      as Seq




-- TODO rename
data OwlPFCmd =
  OwlPFCNewElts [(REltId, OwlSpot, OwlElt)]
  | OwlPFCDeleteElts [(REltId, OwlSpot, OwlElt)]

  | OwlPFCNewTree (MiniOwlTree, OwlSpot)
  | OwlPFCDeleteTree (MiniOwlTree, OwlSpot)

  | OwlPFCManipulate ControllersWithId
  -- we need SuperOwlParliament for undo
  | OwlPFCMove (OwlSpot, SuperOwlParliament)

  | OwlPFCResizeCanvas DeltaLBox

  -- | OwlPFCSnap (OwlPFState, OwlPFState) --(before, after)
  deriving (Show, Generic)

instance NFData OwlPFCmd

-- TODO rename vars
data ActionStack = ActionStack {
  -- TODO change these to Seq
  _actionStack_doStack     :: [OwlPFCmd] -- maybe just do something lke [PFCmd, Maybe OwlPFState] here for state based undo
  , _actionStack_undoStack :: [OwlPFCmd] -- stack of things we've undone
  , _actionStack_lastSaved :: Maybe Int -- size of do stacks on last save
} deriving (Show, Generic)

instance NFData ActionStack

emptyActionStack :: ActionStack
emptyActionStack = ActionStack [] [] (Just 0)

-- UNTESTED
actionStack_hasUnsavedChanges :: ActionStack -> Bool
actionStack_hasUnsavedChanges ActionStack {..} = case _actionStack_lastSaved of
  Nothing -> True
  Just x -> x /= length _actionStack_doStack

-- TODO rename
data OwlPFWorkspace = OwlPFWorkspace {
  -- TODO rename owlPFState
  _owlPFWorkspace_pFState       :: OwlPFState
  , _owlPFWorkspace_lastChanges :: SuperOwlChanges
  , _owlPFWorkspace_actionStack :: ActionStack
} deriving (Show, Generic)

instance NFData OwlPFWorkspace

loadOwlPFStateIntoWorkspace :: OwlPFState -> OwlPFWorkspace -> OwlPFWorkspace
loadOwlPFStateIntoWorkspace pfs ws = r where
  removeOld = fmap (const Nothing) (_owlTree_mapping . _owlPFState_owlTree . _owlPFWorkspace_pFState $ ws)
  addNew = IM.mapWithKey (\rid (oem,oe) -> Just (SuperOwl rid oem oe)) (_owlTree_mapping . _owlPFState_owlTree $ pfs)
  changes = IM.union addNew removeOld
  r = OwlPFWorkspace pfs changes emptyActionStack

emptyWorkspace :: OwlPFWorkspace
emptyWorkspace = OwlPFWorkspace emptyOwlPFState IM.empty emptyActionStack

-- UNTESTED
markWorkspaceSaved :: OwlPFWorkspace -> OwlPFWorkspace
markWorkspaceSaved pfw = r where
  as@ActionStack {..} = _owlPFWorkspace_actionStack pfw
  newas = as { _actionStack_lastSaved = Just (length _actionStack_doStack) }
  r = pfw { _owlPFWorkspace_actionStack = newas }

undoWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
undoWorkspace pfw =  r where
  ActionStack {..} = _owlPFWorkspace_actionStack pfw
  r = case _actionStack_doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ OwlPFWorkspace (undoCmdState c _owlPFWorkspace_pFState) (ActionStack cs (c:_actionStack_undoStack))
    c : cs -> uncurry OwlPFWorkspace (undoCmdState c (_owlPFWorkspace_pFState pfw)) (ActionStack cs (c:_actionStack_undoStack) _actionStack_lastSaved)
    _ -> pfw

redoWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
redoWorkspace pfw = r where
  ActionStack {..} = _owlPFWorkspace_actionStack pfw
  r = case _actionStack_undoStack of
    --c : cs -> trace "REDO: " . traceShow c $ OwlPFWorkspace (doCmdState c _owlPFWorkspace_pFState) (ActionStack (c:_actionStack_doStack) cs)
    c : cs -> uncurry OwlPFWorkspace (doCmdState c (_owlPFWorkspace_pFState pfw)) (ActionStack (c:_actionStack_doStack) cs _actionStack_lastSaved)
    _ -> pfw


undoPermanentWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
undoPermanentWorkspace pfw =  r where
  ActionStack {..} = _owlPFWorkspace_actionStack pfw
  -- NOTE this step is rather unecessary as this is always followed by a doCmdWorkspace but it's best to keep the state correct in between in case anything changes in the future
  newLastSaved = case _actionStack_lastSaved of
    Nothing -> Nothing
    Just x -> if length _actionStack_doStack > x
      -- we are undoing a change that came after last save
      then Just x
      -- we are permanently undoing a change from last saved
      else Nothing
  r = case _actionStack_doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ OwlPFWorkspace (undoCmdState c _owlPFWorkspace_pFState) (ActionStack cs (c:_actionStack_undoStack))
    c : cs -> uncurry OwlPFWorkspace (undoCmdState c (_owlPFWorkspace_pFState pfw)) (ActionStack cs _actionStack_undoStack newLastSaved)
    _ -> pfw

doCmdWorkspace :: OwlPFCmd -> OwlPFWorkspace -> OwlPFWorkspace
--doCmdWorkspace cmd OwlPFWorkspace {..} = trace "DO: " . traceShow cmd $ r wherem
doCmdWorkspace cmd pfw = force r where
  newState = doCmdState cmd (_owlPFWorkspace_pFState pfw)
  ActionStack {..} = (_owlPFWorkspace_actionStack pfw)
  newLastSaved = case _actionStack_lastSaved of
    Nothing -> Nothing
    Just x -> if length _actionStack_doStack < x
      -- we "did" something when last save is still on undo stack, so we can never recover to last saved
      then Nothing
      -- we can still undo back to last save state
      else Just x
  newStack = ActionStack (cmd:_actionStack_doStack) [] newLastSaved
  --newMaxId = owlPFState_maxID _owlPFWorkspace_pFState
  r = uncurry OwlPFWorkspace newState newStack

doCmdState :: OwlPFCmd -> OwlPFState -> (OwlPFState, SuperOwlChanges)
doCmdState cmd s = assert (owlPFState_isValid newState) (newState, changes) where
  (newState, changes) = case cmd of

    OwlPFCNewElts x      ->  do_newElts x s
    OwlPFCDeleteElts x   ->  do_deleteElts x s

    OwlPFCNewTree x -> do_newMiniOwlTree x s
    OwlPFCDeleteTree x -> do_deleteMiniOwlTree x s

    OwlPFCManipulate x   ->  do_manipulate x s
    OwlPFCMove x         -> do_move x s
    OwlPFCResizeCanvas x -> (do_resizeCanvas x s, IM.empty)

undoCmdState :: OwlPFCmd -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undoCmdState cmd s = assert (owlPFState_isValid newState) (newState, changes) where
  (newState, changes) =  case cmd of

    OwlPFCNewElts x      ->  undo_newElts x s
    OwlPFCDeleteElts x   ->  undo_deleteElts x s

    OwlPFCNewTree x -> undo_newMiniOwlTree x s
    OwlPFCDeleteTree x -> undo_deleteMiniOwlTree x s

    OwlPFCManipulate x   ->  undo_manipulate x s
    OwlPFCMove x         -> undo_move x s
    OwlPFCResizeCanvas x -> (undo_resizeCanvas x s, IM.empty)

------ update functions via commands
data WSEvent =
  WSEAddElt (Bool, OwlSpot, OwlElt)

  -- TODO CAN DELETE has been replaced by WSEAddTree
  -- TODO won't work, needs to support OwlElts with kiddos, need MiniOwlTree
  -- it's a little weird that MiniOwlTree is already reindexed though...
  -- maybe just take a selttree D:
  -- I can't remember why I called this WSEAddRelative D:
  | WSEAddRelative (OwlSpot, Seq OwlElt)

  | WSEAddTree (OwlSpot, MiniOwlTree)

  | WSEAddFolder (OwlSpot, Text)
  | WSERemoveElt OwlParliament
  | WSEMoveElt (OwlSpot, OwlParliament)
  -- | WSEDuplicate OwlParliament -- kiddos get duplicated??
  | WSEManipulate (Bool, ControllersWithId)
  | WSEResizeCanvas DeltaLBox
  | WSEUndo
  | WSERedo
  | WSELoad SPotatoFlow
  deriving (Show)

debugPrintBeforeAfterState :: (IsString a) => OwlPFState -> OwlPFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintOwlPFState stateBefore <> "\nAFTER: " <> debugPrintOwlPFState stateAfter


doCmdOwlPFWorkspaceUndoPermanentFirst :: (OwlPFState -> OwlPFCmd) -> OwlPFWorkspace -> OwlPFWorkspace
doCmdOwlPFWorkspaceUndoPermanentFirst cmdFn ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  undoedws = undoPermanentWorkspace ws
  undoedpfs = _owlPFWorkspace_pFState undoedws
  cmd = cmdFn undoedpfs
  r = doCmdWorkspace cmd undoedws


------ helpers for converting events to cmds
-- TODO assert elts are valid
pfc_addElt_to_newElts :: OwlPFState -> OwlSpot -> OwlElt -> OwlPFCmd
pfc_addElt_to_newElts pfs spot oelt = OwlPFCNewElts [(owlPFState_nextId pfs, spot, oelt)]

-- TODO assert elts are valid
pfc_addRelative_to_newElts :: OwlPFState -> (OwlSpot, Seq OwlElt) -> OwlPFCmd
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
  r = OwlPFCDeleteElts $ toList (fmap (\SuperOwl {..} -> (_superOwl_id, owlTree_owlEltMeta_toOwlSpot od _superOwl_meta, _superOwl_elt)) sowlswithchildren)

pfc_addFolder_to_newElts :: OwlPFState -> (OwlSpot, Text) -> OwlPFCmd
pfc_addFolder_to_newElts pfs (spot, name) = OwlPFCNewElts [(owlPFState_nextId pfs, spot, OwlEltFolder (OwlInfo name) Seq.empty)]

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
      msbox = getSEltBox $ superOwl_toSElt_hack sowl
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
    WSEManipulate (undo, x) -> if undo
      then doCmdOwlPFWorkspaceUndoPermanentFirst (\pfs -> pfc_manipulate_to_manipulate pfs x) ws
      else doCmdWorkspace (pfc_manipulate_to_manipulate lastState x) ws
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
