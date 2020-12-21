{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Workspace (
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
) where

import           Relude

import           Potato.Flow.Cmd
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Control.Exception  (assert)
import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence      as Seq

-- TODO move this into a diff file
data ActionStack = ActionStack {
  doStack     :: [PFCmd] -- maybe just do something lke [PFCmd, Maybe PFState] here for state based undo
  , undoStack :: [PFCmd]
} deriving (Generic)

instance NFData ActionStack

emptyActionStack :: ActionStack
emptyActionStack = ActionStack [] []

data PFWorkspace = PFWorkspace {
  _pFWorkspace_state         :: PFState
  , _pFWorkspace_lastChanges :: SEltLabelChanges
  , _pFWorkspace_actionStack :: ActionStack
} deriving (Generic)

instance NFData PFWorkspace

loadPFStateIntoWorkspace :: PFState -> PFWorkspace -> PFWorkspace
loadPFStateIntoWorkspace pfs ws = r where
  removeOld = fmap (const Nothing) (_pFState_directory . _pFWorkspace_state $ ws)
  addNew = fmap Just (_pFState_directory pfs)
  changes = IM.union addNew removeOld
  r = PFWorkspace pfs changes emptyActionStack

emptyWorkspace :: PFWorkspace
emptyWorkspace = PFWorkspace emptyPFState IM.empty emptyActionStack

undoWorkspace :: PFWorkspace -> PFWorkspace
undoWorkspace pfw =  r where
  ActionStack {..} = _pFWorkspace_actionStack pfw
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ PFWorkspace (undoCmdState c _pFWorkspace_state) (ActionStack cs (c:undoStack))
    c : cs -> uncurry PFWorkspace (undoCmdState c (_pFWorkspace_state pfw)) (ActionStack cs (c:undoStack))
    _ -> pfw

redoWorkspace :: PFWorkspace -> PFWorkspace
redoWorkspace pfw = r where
  ActionStack {..} = _pFWorkspace_actionStack pfw
  r = case undoStack of
    --c : cs -> trace "REDO: " . traceShow c $ PFWorkspace (doCmdState c _pFWorkspace_state) (ActionStack (c:doStack) cs)
    c : cs -> uncurry PFWorkspace (doCmdState c (_pFWorkspace_state pfw)) (ActionStack (c:doStack) cs)
    _ -> pfw


undoPermanentWorkspace :: PFWorkspace -> PFWorkspace
undoPermanentWorkspace pfw =  r where
  ActionStack {..} = _pFWorkspace_actionStack pfw
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ PFWorkspace (undoCmdState c _pFWorkspace_state) (ActionStack cs (c:undoStack))
    c : cs -> uncurry PFWorkspace (undoCmdState c (_pFWorkspace_state pfw)) (ActionStack cs undoStack)
    _ -> pfw

doCmdWorkspace :: PFCmd -> PFWorkspace -> PFWorkspace
--doCmdWorkspace cmd PFWorkspace {..} = trace "DO: " . traceShow cmd $ r wherem
-- deepseq here to force evaluation of workspace and prevent leaks
doCmdWorkspace cmd pfw = force r where
  newState = doCmdState cmd (_pFWorkspace_state pfw)
  ActionStack {..} = (_pFWorkspace_actionStack pfw)
  newStack = ActionStack (cmd:doStack) []
  --newMaxId = pFState_maxID _pFWorkspace_state
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

pfc_paste_to_newElts :: PFState -> ([SEltLabel], LayerPos) -> PFCmd
pfc_paste_to_newElts pfs (seltls, lp) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> zip3 [rid..] [lp..] seltls

--pfc_duplicate_to_duplicate :: PFState -> [LayerPos] -> PFCmd
--pfc_duplicate_to_duplicate pfs lps = r where
--  rids = map (Seq.index _pFState_layers) lps
--  r = PFCFDuplicate ==> rids
