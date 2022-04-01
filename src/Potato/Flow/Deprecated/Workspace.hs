{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Deprecated.Workspace (
  PFWorkspace(..)
  , emptyWorkspace
  , emptyActionStack
  , loadPFStateIntoWorkspace
  , undoWorkspace
  , redoWorkspace
  , undoPermanentWorkspace
  , doCmdWorkspace
  , pfc_addElt_to_newElts
  , pfc_addFolder_to_newElts
  , pfc_removeElt_to_deleteElts
  , pfc_paste_to_newElts
  , WSEvent(..)
  , updatePFWorkspace
) where

import           Relude

import           Potato.Flow.Cmd
import           Potato.Flow.Deprecated.Layers
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Deprecated.State
import           Potato.Flow.Types

import           Control.Exception  (assert)
import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence      as Seq

-- TODO move this into a diff file
data ActionStack = ActionStack {
  doStack     :: [PFCmd] -- maybe just do something lke [PFCmd, Maybe PFState] here for state based undo
  , undoStack :: [PFCmd]
} deriving (Show, Eq, Generic)

instance NFData ActionStack

emptyActionStack :: ActionStack
emptyActionStack = ActionStack [] []

data PFWorkspace = PFWorkspace {
  -- TODO rename to pFState
  _pFWorkspace_pFState       :: PFState
  , _pFWorkspace_lastChanges :: SEltLabelChanges
  , _pFWorkspace_actionStack :: ActionStack
} deriving (Show, Eq, Generic)

instance NFData PFWorkspace

loadPFStateIntoWorkspace :: PFState -> PFWorkspace -> PFWorkspace
loadPFStateIntoWorkspace pfs ws = r where
  removeOld = fmap (const Nothing) (_pFState_directory . _pFWorkspace_pFState $ ws)
  addNew = fmap Just (_pFState_directory pfs)
  changes = IM.union addNew removeOld
  r = PFWorkspace pfs changes emptyActionStack

emptyWorkspace :: PFWorkspace
emptyWorkspace = PFWorkspace emptyPFState IM.empty emptyActionStack

undoWorkspace :: PFWorkspace -> PFWorkspace
undoWorkspace pfw =  r where
  ActionStack {..} = _pFWorkspace_actionStack pfw
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ PFWorkspace (undoCmdState c _pFWorkspace_pFState) (ActionStack cs (c:undoStack))
    c : cs -> uncurry PFWorkspace (undoCmdState c (_pFWorkspace_pFState pfw)) (ActionStack cs (c:undoStack))
    _ -> pfw

redoWorkspace :: PFWorkspace -> PFWorkspace
redoWorkspace pfw = r where
  ActionStack {..} = _pFWorkspace_actionStack pfw
  r = case undoStack of
    --c : cs -> trace "REDO: " . traceShow c $ PFWorkspace (doCmdState c _pFWorkspace_pFState) (ActionStack (c:doStack) cs)
    c : cs -> uncurry PFWorkspace (doCmdState c (_pFWorkspace_pFState pfw)) (ActionStack (c:doStack) cs)
    _ -> pfw

undoPermanentWorkspace :: PFWorkspace -> PFWorkspace
undoPermanentWorkspace pfw =  r where
  ActionStack {..} = _pFWorkspace_actionStack pfw
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ PFWorkspace (undoCmdState c _pFWorkspace_pFState) (ActionStack cs (c:undoStack))
    c : cs -> uncurry PFWorkspace (undoCmdState c (_pFWorkspace_pFState pfw)) (ActionStack cs undoStack)
    _ -> pfw

doCmdWorkspace :: PFCmd -> PFWorkspace -> PFWorkspace
--doCmdWorkspace cmd PFWorkspace {..} = trace "DO: " . traceShow cmd $ r wherem
-- deepseq here to force evaluation of workspace and prevent leaks
doCmdWorkspace cmd pfw = force r where
  newState = doCmdState cmd (_pFWorkspace_pFState pfw)
  ActionStack {..} = (_pFWorkspace_actionStack pfw)
  newStack = ActionStack (cmd:doStack) []
  --newMaxId = pFState_maxID _pFWorkspace_pFState
  r = uncurry PFWorkspace newState newStack

doCmdState :: PFCmd -> PFState -> (PFState, SEltLabelChanges)
doCmdState cmd s = assert (pFState_isValid newState) (newState, changes) where
  (newState, changes) = case cmd of
    (PFCNewElts :=> Identity x)      ->  do_newElts x s
    (PFCDeleteElts :=> Identity x)   ->  do_deleteElts x s
    (PFCManipulate :=> Identity x)   ->  do_manipulate x s
    (PFCMove :=> Identity x)         -> do_move x s
    (PFCResizeCanvas :=> Identity x) -> (do_resizeCanvas x s, IM.empty)

undoCmdState :: PFCmd -> PFState -> (PFState, SEltLabelChanges)
undoCmdState cmd s = assert (pFState_isValid newState) (newState, changes) where
  (newState, changes) =  case cmd of
    (PFCNewElts :=> Identity x)      ->  undo_newElts x s
    (PFCDeleteElts :=> Identity x)   ->  undo_deleteElts x s
    (PFCManipulate :=> Identity x)   ->  undo_manipulate x s
    (PFCMove :=> Identity x)         -> undo_move x s
    (PFCResizeCanvas :=> Identity x) -> (undo_resizeCanvas x s, IM.empty)

------ helpers for converting events to cmds
-- TODO move these to a different file prob
pfc_addElt_to_newElts :: PFState -> (LayerPos, SEltLabel) -> PFCmd
pfc_addElt_to_newElts pfs (lp,seltl) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> [(rid,lp,seltl)]

pfc_addFolder_to_newElts :: PFState -> (LayerPos, Text) -> PFCmd
pfc_addFolder_to_newElts pfs (lp, name) = r where
  ridStart = pFState_maxID pfs + 1
  ridEnd = ridStart + 1
  seltlStart = SEltLabel name SEltFolderStart
  seltlEnd = SEltLabel (name <> " (end)") SEltFolderEnd
  r = PFCNewElts ==> [(ridStart, lp, seltlStart), (ridEnd, lp+1, seltlEnd)]

debugPrintLayerPoss :: (IsString a) => PFState -> [LayerPos] -> a
debugPrintLayerPoss PFState {..} lps = fromString msg where
  rids = map (Seq.index _pFState_layers) lps
  seltls = map ((IM.!) _pFState_directory) rids
  msg = show $ (zip3 rids lps (map _sEltLabel_sElt seltls))

-- TODO consider including folder end to selecetion if not included
-- or at least assert to ensure it's correct
pfc_removeElt_to_deleteElts :: PFState -> [LayerPos] -> PFCmd
--pfc_removeElt_to_deleteElts pfs@PFState {..} lps = if length lps > 1 then trace (debugPrintLayerPoss pfs lps) r else r where
pfc_removeElt_to_deleteElts PFState {..} lps = r where
  rids = map (Seq.index _pFState_layers) lps
  seltls = map ((IM.!) _pFState_directory) rids
  r = PFCDeleteElts ==> (zip3 rids lps seltls)

-- TODO DELETE
pfc_paste_to_newElts :: PFState -> ([SEltLabel], LayerPos) -> PFCmd
pfc_paste_to_newElts pfs (seltls, lp) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> zip3 [rid..] [lp..] seltls

pfc_addRelative_to_newElts :: PFState -> (LayerPos, SEltTree) -> PFCmd
pfc_addRelative_to_newElts pfs (lp, stree) = assert validScope $ r where
  validScope = selectionHasScopingProperty scopeFn (Seq.fromList stree) [0..length stree - 1]
  scopeFn (_,seltl) = case seltl of
    (SEltLabel _ SEltFolderStart) -> Just True
    (SEltLabel _ SEltFolderEnd)   -> Just False
    _                             -> Nothing
  -- TODO reposition/offset (could just offset by 1? or maybe need to add new arg)
  -- TODO reindex SEltTree maintaing connections
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> zip3 [rid..] [lp..] (fmap snd stree)

--pfc_duplicate_to_duplicate :: PFState -> [LayerPos] -> PFCmd
--pfc_duplicate_to_duplicate pfs lps = r where
--  rids = map (Seq.index _pFState_layers) lps
--  r = PFCFDuplicate ==> rids

------ update functions via commands
data WSEvent =
  -- CHANGE TODO FIGURE IT OUT
  --WSEAddElt (Bool, OwlSpot, OwlItem)
  -- | WSEAddRelative (OwlSpot, Seq OwlItem)
  -- | WSEAddFolder (OwlSpot, Text)
  -- | WSERemoveElt [REltId] -- removed kiddos get adopted by grandparents or w/e?
  -- | WSEMoveElt (OwlSpot, [REltId]) -- also moves kiddos?
  -- | WSEDuplicate [REltId] -- kiddos get duplicated??


  WSEAddElt (Bool, (LayerPos, SEltLabel))
  | WSEAddRelative (LayerPos, SEltTree)
  | WSEAddFolder (LayerPos, Text)
  | WSERemoveElt [LayerPos]
  | WSEMoveElt ([LayerPos], LayerPos)
  -- | WSEDuplicate [LayerPos]
  | WSEManipulate (Bool, ControllersWithId)
  | WSEResizeCanvas DeltaLBox
  | WSEUndo
  | WSERedo
  | WSELoad SPotatoFlow
  deriving (Show, Eq)

debugPrintBeforeAfterState :: (IsString a) => PFState -> PFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintPFState stateBefore <> "\nAFTER: " <> debugPrintPFState stateAfter

doCmdPFWorkspaceUndoPermanentFirst :: (PFState -> PFCmd) -> PFWorkspace -> PFWorkspace
doCmdPFWorkspaceUndoPermanentFirst cmdFn ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  undoedws = undoPermanentWorkspace ws
  undoedpfs = _pFWorkspace_pFState undoedws
  cmd = cmdFn undoedpfs
  r = doCmdWorkspace cmd undoedws

updatePFWorkspace :: WSEvent -> PFWorkspace -> PFWorkspace
updatePFWorkspace evt ws = let
  lastState = _pFWorkspace_pFState ws
  r = case evt of
    WSEAddElt (undo, x) -> if undo
      then doCmdPFWorkspaceUndoPermanentFirst (\pfs -> pfc_addElt_to_newElts pfs x) ws
      else doCmdWorkspace (pfc_addElt_to_newElts lastState x) ws
    WSEAddRelative x -> doCmdWorkspace (pfc_addRelative_to_newElts lastState x) ws
    WSEAddFolder x -> doCmdWorkspace (pfc_addFolder_to_newElts lastState x) ws
    WSERemoveElt x -> doCmdWorkspace (pfc_removeElt_to_deleteElts lastState x) ws
    WSEManipulate (undo, x) -> if undo
      then doCmdPFWorkspaceUndoPermanentFirst (const (PFCManipulate ==> x)) ws
      else doCmdWorkspace (PFCManipulate ==> x) ws
    -- TODO add children to selection before moving
    WSEMoveElt x -> doCmdWorkspace (PFCMove ==> x) ws
    WSEResizeCanvas x -> doCmdWorkspace (PFCResizeCanvas ==> x) ws
    WSEUndo -> undoWorkspace ws
    WSERedo -> redoWorkspace ws
    WSELoad x -> loadPFStateIntoWorkspace (sPotatoFlow_to_pFState x) ws
  afterState = _pFWorkspace_pFState r
  isValidAfter = pFState_isValid afterState
  in
    if isValidAfter then r else
      error ("INVALID " <> show evt <> "\n" <> debugPrintBeforeAfterState lastState afterState)
