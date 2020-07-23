{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.New.Workspace (
  PFWorkspace(..)
  , emptyWorkspace
  , emptyActionStack
  , workspaceFromState
  , undoWorkspace
  , redoWorkspace
  , doCmdWorkspaceUndoFirst
  , doCmdWorkspace
  , pfc_addElt_to_newElts
  , pfc_addFolder_to_newElts
  , pfc_removeElt_to_deleteElts
  , pfc_paste_to_newElts
) where

import           Relude

import           Potato.Flow.New.Cmd
import           Potato.Flow.New.State
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Data.Constraint.Extras   (Has')
import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict       as IM
import qualified Data.List.NonEmpty       as NE
import qualified Data.Sequence            as Seq

-- TODO move this into a diff file
data ActionStack = ActionStack {
  doStack     :: [PFCmd]
  , undoStack :: [PFCmd]
}

emptyActionStack :: ActionStack
emptyActionStack = ActionStack [] []

data PFWorkspace = PFWorkspace {
  _pFWorkspace_state         :: PFState
  , _pFWorkspace_actionStack :: ActionStack
  -- TODO this isn't needed delete
  --, _pFWorkspace_maxId       :: REltId
}

workspaceFromState :: PFState -> PFWorkspace
workspaceFromState s = PFWorkspace s emptyActionStack

emptyWorkspace :: PFWorkspace
emptyWorkspace = PFWorkspace emptyPFState emptyActionStack

undoWorkspace :: PFWorkspace -> PFWorkspace
undoWorkspace pfw@PFWorkspace {..} = r where
  ActionStack {..} = _pFWorkspace_actionStack
  r = case doStack of
    --c : cs -> trace "UNDO: " .traceShow c $ PFWorkspace (undoCmdState c _pFWorkspace_state) (ActionStack cs (c:undoStack))
    c : cs -> PFWorkspace (undoCmdState c _pFWorkspace_state) (ActionStack cs (c:undoStack))
    _ -> pfw

redoWorkspace :: PFWorkspace -> PFWorkspace
redoWorkspace pfw@PFWorkspace {..} = r where
  ActionStack {..} = _pFWorkspace_actionStack
  r = case undoStack of
    --c : cs -> trace "REDO: " . traceShow c $ PFWorkspace (doCmdState c _pFWorkspace_state) (ActionStack (c:doStack) cs)
    c : cs -> PFWorkspace (doCmdState c _pFWorkspace_state) (ActionStack (c:doStack) cs)
    _ -> pfw


doCmdWorkspaceUndoFirst :: PFCmd -> PFWorkspace -> PFWorkspace
doCmdWorkspaceUndoFirst cmd ws = doCmdWorkspace cmd (undoWorkspace ws)

doCmdWorkspace :: PFCmd -> PFWorkspace -> PFWorkspace
--doCmdWorkspace cmd PFWorkspace {..} = trace "DO: " . traceShow cmd $ r where
doCmdWorkspace cmd PFWorkspace {..} = r where
  newState = doCmdState cmd _pFWorkspace_state
  ActionStack {..} = _pFWorkspace_actionStack
  newStack = ActionStack (cmd:doStack) []
  --newMaxId = pFState_maxID _pFWorkspace_state
  r = PFWorkspace newState newStack

{-
data PFCmdTag t a where
  -- LayerPos indices are as if all elements already exist in the map
  PFCNewElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  -- LayerPos indices are the current indices of elements to be removed
  PFCDeleteElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  --PFCMove :: PFCmdTag t (LayerPos, NonEmpty LayerPos)
  PFCManipulate :: PFCmdTag t (ControllersWithId)
  PFCResizeCanvas :: PFCmdTag t DeltaLBox
-}

-- TODO you probably want to output something like this as well
--_sEltLayerTree_changeView :: REltIdMap (MaybeMaybe SEltLabel)

doCmdState :: PFCmd -> PFState -> PFState
doCmdState cmd state = case cmd of
  (PFCNewElts :=> Identity x)      ->  do_newElts x state
  (PFCDeleteElts :=> Identity x)   ->  do_deleteElts x state
  (PFCManipulate :=> Identity x)   ->  do_manipulate x state
  (PFCResizeCanvas :=> Identity x) -> do_resizeCanvas x state
  _                                -> undefined

undoCmdState :: PFCmd -> PFState -> PFState
undoCmdState cmd state = case cmd of
  (PFCNewElts :=> Identity x)      ->  undo_newElts x state
  (PFCDeleteElts :=> Identity x)   ->  undo_deleteElts x state
  (PFCManipulate :=> Identity x)   ->  undo_manipulate x state
  (PFCResizeCanvas :=> Identity x) -> undo_resizeCanvas x state
  _                                -> undefined



------ helpers for converting events to cmds
-- TODO move these to a different file prob
pfc_addElt_to_newElts :: PFState -> (LayerPos, SEltLabel) -> PFCmd
pfc_addElt_to_newElts pfs (lp,seltl) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> ((rid,lp,seltl) NE.:| [])

pfc_addFolder_to_newElts :: PFState -> (LayerPos, Text) -> PFCmd
pfc_addFolder_to_newElts pfs (lp, name) = r where
  ridStart = pFState_maxID pfs + 1
  ridEnd = ridStart + 1
  seltlStart = SEltLabel name SEltFolderStart
  seltlEnd = SEltLabel (name <> " (end)") SEltFolderEnd
  r = PFCNewElts ==> NE.fromList [(ridStart, lp, seltlStart), (ridEnd, lp, seltlEnd)]

-- TODO consider including folder end to selecetion if not included
pfc_removeElt_to_deleteElts :: PFState -> [LayerPos] -> PFCmd
pfc_removeElt_to_deleteElts PFState {..} lps = r where
  rids = map (Seq.index _pFState_layers) lps
  seltls = map ((IM.!) _pFState_directory) rids
  r = PFCDeleteElts ==> NE.fromList (zip3 rids lps seltls)

--pfc_moveElt_to_move :: PFState -> ([LayerPos], LayerPos) -> PFCmd
--pfc_moveElt_to_move pfs x = r where
--  r = PFCMove ==> x

pfc_paste_to_newElts :: PFState -> ([SEltLabel], LayerPos) -> PFCmd
pfc_paste_to_newElts pfs (seltls, lp) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> NE.fromList (zip3 [rid..] [lp..] seltls)

--pfc_duplicate_to_duplicate :: PFState -> [LayerPos] -> PFCmd
--pfc_duplicate_to_duplicate pfs lps = r where
--  rids = map (Seq.index _pFState_layers) lps
--  r = PFCFDuplicate ==> rids
