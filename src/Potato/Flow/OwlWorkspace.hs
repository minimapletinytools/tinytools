{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.OwlWorkspace (
  OwlPFWorkspace(..)
  , emptyWorkspace
  , emptyActionStack
  , undoWorkspace
  , redoWorkspace
  , undoPermanentWorkspace
  , doCmdWorkspace
  , WSEvent(..)
  , updateOwlPFWorkspace
) where

import           Relude

import           Potato.Flow.Cmd
import           Potato.Flow.Layers
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Owl
import           Potato.Flow.OwlState
import           Potato.Flow.Types

import           Control.Exception  (assert)
import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence      as Seq



data OwlPFCmd =
  OwlPFCNewElts [(REltId, OwlSpot, OwlElt)]
  | OwlPFCDeleteElts [(REltId, OwlSpot, OwlElt)]
  | OwlPFCManipulate ControllersWithId
  -- we need SuperOwlParliament for undo
  | OwlPFCMove (OwlSpot, SuperOwlParliament)

  | OwlPFCResizeCanvas DeltaLBox
  deriving (Show, Generic)

instance NFData OwlPFCmd

data ActionStack = ActionStack {
  doStack     :: [OwlPFCmd] -- maybe just do something lke [PFCmd, Maybe OwlPFState] here for state based undo
  , undoStack :: [OwlPFCmd]
} deriving (Show, Generic)

instance NFData ActionStack

emptyActionStack :: ActionStack
emptyActionStack = ActionStack [] []

data OwlPFWorkspace = OwlPFWorkspace {
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
emptyWorkspace = OwlPFWorkspace emptyPFState IM.empty emptyActionStack

undoWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
undoWorkspace pfw =  r where
  ActionStack {..} = _owlPFWorkspace_actionStack pfw
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ OwlPFWorkspace (undoCmdState c _owlPFWorkspace_pFState) (ActionStack cs (c:undoStack))
    c : cs -> uncurry OwlPFWorkspace (undoCmdState c (_owlPFWorkspace_pFState pfw)) (ActionStack cs (c:undoStack))
    _ -> pfw

redoWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
redoWorkspace pfw = r where
  ActionStack {..} = _owlPFWorkspace_actionStack pfw
  r = case undoStack of
    --c : cs -> trace "REDO: " . traceShow c $ OwlPFWorkspace (doCmdState c _owlPFWorkspace_pFState) (ActionStack (c:doStack) cs)
    c : cs -> uncurry OwlPFWorkspace (doCmdState c (_owlPFWorkspace_pFState pfw)) (ActionStack (c:doStack) cs)
    _ -> pfw

undoPermanentWorkspace :: OwlPFWorkspace -> OwlPFWorkspace
undoPermanentWorkspace pfw =  r where
  ActionStack {..} = _owlPFWorkspace_actionStack pfw
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ OwlPFWorkspace (undoCmdState c _owlPFWorkspace_pFState) (ActionStack cs (c:undoStack))
    c : cs -> uncurry OwlPFWorkspace (undoCmdState c (_owlPFWorkspace_pFState pfw)) (ActionStack cs undoStack)
    _ -> pfw

doCmdWorkspace :: OwlPFCmd -> OwlPFWorkspace -> OwlPFWorkspace
--doCmdWorkspace cmd OwlPFWorkspace {..} = trace "DO: " . traceShow cmd $ r wherem
doCmdWorkspace cmd pfw = force r where
  newState = doCmdState cmd (_owlPFWorkspace_pFState pfw)
  ActionStack {..} = (_owlPFWorkspace_actionStack pfw)
  newStack = ActionStack (cmd:doStack) []
  --newMaxId = pFState_maxID _owlPFWorkspace_pFState
  r = uncurry OwlPFWorkspace newState newStack

doCmdState :: OwlPFCmd -> OwlPFState -> (OwlPFState, SuperOwlChanges)
doCmdState cmd s = assert (pFState_isValid newState) (newState, changes) where
  (newState, changes) = case cmd of
    OwlPFCNewElts x      ->  do_newElts x s
    OwlPFCDeleteElts x   ->  do_deleteElts x s
    OwlPFCManipulate x   ->  do_manipulate x s
    OwlPFCMove x         -> do_move x s
    OwlPFCResizeCanvas x -> (do_resizeCanvas x s, IM.empty)

undoCmdState :: OwlPFCmd -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undoCmdState cmd s = assert (pFState_isValid newState) (newState, changes) where
  (newState, changes) =  case cmd of
    OwlPFCNewElts x      ->  undo_newElts x s
    OwlPFCDeleteElts x   ->  undo_deleteElts x s
    OwlPFCManipulate x   ->  undo_manipulate x s
    OwlPFCMove x         -> undo_move x s
    OwlPFCResizeCanvas x -> (undo_resizeCanvas x s, IM.empty)

------ update functions via commands
data WSEvent =
  WSEAddElt (Bool, OwlSpot, OwlElt)
  | WSEAddRelative (OwlSpot, Seq OwlElt)
  | WSEAddFolder (OwlSpot, Text)
  | WSERemoveElt OwlParliament -- removed kiddos get adopted by grandparents or w/e?
  | WSEMoveElt (OwlSpot, SuperOwlParliament) -- use SuperOwlParliament so we know where to undo back to
  -- | WSEDuplicate OwlParliament -- kiddos get duplicated??
  | WSEManipulate (Bool, ControllersWithId)
  | WSEResizeCanvas DeltaLBox
  | WSEUndo
  | WSERedo
  | WSELoad SPotatoFlow
  deriving (Show)

debugPrintBeforeAfterState :: (IsString a) => OwlPFState -> OwlPFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintPFState stateBefore <> "\nAFTER: " <> debugPrintPFState stateAfter

doCmdOwlPFWorkspaceUndoPermanentFirst :: (OwlPFState -> OwlPFCmd) -> OwlPFWorkspace -> OwlPFWorkspace
doCmdOwlPFWorkspaceUndoPermanentFirst cmdFn ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  undoedws = undoPermanentWorkspace ws
  undoedpfs = _owlPFWorkspace_pFState undoedws
  cmd = cmdFn undoedpfs
  r = doCmdWorkspace cmd undoedws

------ helpers for converting events to cmds
pfc_addElt_to_newElts :: OwlPFState -> OwlSpot -> OwlElt -> OwlPFCmd
pfc_addElt_to_newElts pfs spot oelt = OwlPFCNewElts [(owlPFState_nextId pfs, spot, oelt)]

-- UNTESTED
pfc_addRelative_to_newElts :: OwlPFState -> (OwlSpot, Seq OwlElt) -> OwlPFCmd
pfc_addRelative_to_newElts pfs (ospot, oelts) = r where
  startid = owlPFState_nextId pfs + 1
  mapAccumLFn (i,ospotacc) oelt = ((i+1, nextacc), (rid, ospotacc, oelt)) where
    -- order from left to right so there is valid leftSibling
    nextacc = ospotacc { _owlSpot_leftSibling = Just rid}
    rid = startid + i
  (_, r') = mapAccumL mapAccumLFn (0,ospot) oelts
  r = OwlPFCNewElts $ toList r'

-- UNTESTED
pfc_removeElt_to_deleteElts :: OwlPFState -> OwlParliament -> OwlPFCmd
pfc_removeElt_to_deleteElts pfs owlp = r where
  od = _owlPFState_owlTree pfs
  SuperOwlParliament sowlp = owlParliament_toSuperOwlParliament od owlp
  getrpos x = _owlEltMeta_relPosition $ _superOwl_meta x
  -- order from left to right so that leftSibling is always valid (we delete from the right)
  r' = Seq.sortBy (\a b -> compare (getrpos a) (getrpos b)) sowlp
  r = OwlPFCDeleteElts $ toList (fmap (\SuperOwl {..} -> (_superOwl_id, owlEltMeta_toOwlSpot od _superOwl_meta, _superOwl_elt)) r')

pfc_addFolder_to_newElts :: OwlPFState -> (OwlSpot, Text) -> OwlPFCmd
pfc_addFolder_to_newElts pfs (spot, name) = OwlPFCNewElts [(owlPFState_nextId pfs, spot, OwlEltFolder (OwlInfo name) Seq.empty)]

updateOwlPFWorkspace :: WSEvent -> OwlPFWorkspace -> OwlPFWorkspace
updateOwlPFWorkspace evt ws = let
  lastState = _owlPFWorkspace_pFState ws
  r = case evt of
    WSEAddElt (undo, spot, oelt) -> if undo
      then doCmdOwlPFWorkspaceUndoPermanentFirst (\pfs -> pfc_addElt_to_newElts pfs spot oelt) ws
      else doCmdWorkspace (pfc_addElt_to_newElts lastState spot oelt) ws
    WSEAddRelative x -> doCmdWorkspace (pfc_addRelative_to_newElts lastState x) ws
    WSEAddFolder x -> doCmdWorkspace (pfc_addFolder_to_newElts lastState x) ws
    WSERemoveElt x -> doCmdWorkspace (pfc_removeElt_to_deleteElts lastState x) ws
    WSEManipulate (undo, x) -> if undo
      then doCmdOwlPFWorkspaceUndoPermanentFirst (const (OwlPFCManipulate x)) ws
      else doCmdWorkspace (OwlPFCManipulate x) ws
    WSEMoveElt x -> doCmdWorkspace (OwlPFCMove x) ws
    WSEResizeCanvas x -> doCmdWorkspace (OwlPFCResizeCanvas x) ws
    WSEUndo -> undoWorkspace ws
    WSERedo -> redoWorkspace ws
    WSELoad x -> loadOwlPFStateIntoWorkspace (sPotatoFlow_to_pFState x) ws
  afterState = _owlPFWorkspace_pFState r
  isValidAfter = pFState_isValid afterState
  in
    if isValidAfter then r else
      error ("INVALID " <> show evt <> "\n" <> debugPrintBeforeAfterState lastState afterState)
