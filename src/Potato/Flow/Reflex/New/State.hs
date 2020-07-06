{-# LANGUAGE RecordWildCards #-}
module Potato.Flow.Reflex.New.State (
) where

import           Relude


import           Potato.Flow.Math
import           Potato.Flow.Reflex.New.Cmd
import           Potato.Flow.Reflex.New.Layers
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Reflex

import           Data.Aeson
import           Data.Dependent.Sum            (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict            as IM
import qualified Data.List.NonEmpty            as NE
import qualified Data.List.Safe                as SL
import qualified Data.Sequence                 as Seq


-- TODO move this into a diff file
data ActionStack t = ActionStack {
  doStack     :: [PFCmd t]
  , undoStack :: [PFCmd t]
}

emptyActionStack :: ActionStack t
emptyActionStack = ActionStack [] []

data PFWorkspace t = PFWorkspace {
  _pFWorkspace_state         :: PFState
  , _pFWorkspace_actionStack :: ActionStack t
  -- TODO this isn't needed delete
  , _pFWorkspace_maxId       :: REltId
}

workspaceFromState :: PFState -> PFWorkspace t
workspaceFromState s = PFWorkspace s emptyActionStack $ pFState_maxID s

data PFState = PFState {
  _pFState_layers      :: Seq REltId
  -- TODO cache REltId -> Layers map with dirty flag probably
  , _pFState_directory :: IM.IntMap SEltLabel
  , _pFState_canvas    :: LBox
} deriving (Show, Generic)

instance FromJSON PFState
instance ToJSON PFState
instance NFData PFState

pFState_maxID :: PFState -> REltId
pFState_maxID s = maybe 0 fst (IM.lookupMax (_pFState_directory s))

undoWorkspace :: PFWorkspace t -> PFWorkspace t
undoWorkspace pfw@PFWorkspace {..} = r where
  ActionStack {..} = _pFWorkspace_actionStack
  r = case undoStack of
    c : cs -> PFWorkspace (undoCmdStateState c _pFWorkspace_state) (ActionStack (c:doStack) cs) _pFWorkspace_maxId
    _ -> pfw

doCmdWorkspaceUndoFirst :: PFCmd t -> PFWorkspace t -> PFWorkspace t
doCmdWorkspaceUndoFirst cmd ws = doCmdWorkspace cmd (undoWorkspace ws)

doCmdWorkspace :: PFCmd t -> PFWorkspace t -> PFWorkspace t
doCmdWorkspace cmd PFWorkspace {..} = r where
  newState = doCmdState cmd _pFWorkspace_state
  ActionStack {..} = _pFWorkspace_actionStack
  newStack = ActionStack (cmd:doStack) undoStack
  newMaxId = pFState_maxID _pFWorkspace_state
  r = PFWorkspace newState newStack newMaxId

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

doCmdState :: PFCmd t -> PFState -> PFState
doCmdState cmd state = case cmd of
  (PFCNewElts :=> Identity x)    ->  do_newElts x state
  (PFCDeleteElts :=> Identity x) ->  do_deleteElts x state
  _                              -> undefined

undoCmdStateState :: PFCmd t -> PFState -> PFState
undoCmdStateState cmd state = case cmd of
  (PFCNewElts :=> Identity x)    ->  undo_newElts x state
  (PFCDeleteElts :=> Identity x) ->  undo_deleteElts x state
  _                              -> undefined

do_newElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
do_newElts seltls' PFState {..} = r where
  seltls = toList seltls'
  poss = map (\(x,y,_) -> (y,x)) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  newLayers = insertEltList poss _pFState_layers
  newDir = IM.fromList els `IM.union` _pFState_directory
  r = PFState newLayers newDir _pFState_canvas

undo_newElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
undo_newElts seltls' PFState {..} = r where
  seltls = toList seltls'
  poss = map (\(_,y,_) -> y) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  newLayers = removeEltList poss _pFState_layers
  newDir = _pFState_directory `IM.difference` fromList els
  r = PFState newLayers newDir _pFState_canvas

do_deleteElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
do_deleteElts = undo_newElts

undo_deleteElts :: NonEmpty SuperSEltLabel -> PFState -> PFState
undo_deleteElts = do_newElts

do_resizeCanvas :: DeltaLBox -> PFState -> PFState
do_resizeCanvas d pfs = pfs { _pFState_canvas = plusDelta (_pFState_canvas pfs) d }

undo_resizeCanvas :: DeltaLBox -> PFState -> PFState
undo_resizeCanvas d pfs = pfs { _pFState_canvas = minusDelta (_pFState_canvas pfs) d }

-- TODO
do_manipulate :: ControllersWithId -> PFState -> PFState
do_manipulate c = undefined



------ helpers for converting events to cmds

pfc_addElt_to_newElts :: PFState -> (LayerPos, SEltLabel) -> PFCmd t
pfc_addElt_to_newElts pfs (lp,seltl) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> ((rid,lp,seltl) NE.:| [])

pfc_addFolder_to_newElts :: PFState -> (LayerPos, Text) -> PFCmd t
pfc_addFolder_to_newElts pfs (lp, name) = r where
  ridStart = pFState_maxID pfs + 1
  ridEnd = ridStart + 1
  seltlStart = SEltLabel name SEltFolderStart
  seltlEnd = SEltLabel (name <> " (end)") SEltFolderEnd
  r = PFCNewElts ==> NE.fromList [(ridStart, lp, seltlStart), (ridEnd, lp+1, seltlEnd)]

-- TODO consider including folder end to selecetion if not included
pfc_removeElt_to_deleteElts :: PFState -> [LayerPos] -> PFCmd t
pfc_removeElt_to_deleteElts PFState {..} lps = r where
  rids = map (Seq.index _pFState_layers) lps
  seltls = map ((IM.!) _pFState_directory) rids
  r = PFCDeleteElts ==> NE.fromList (zip3 rids lps seltls)

--pfc_moveElt_to_move :: PFState -> ([LayerPos], LayerPos) -> PFCmd t
--pfc_moveElt_to_move pfs x = r where
--  r = PFCMove ==> x

pfc_paste_to_newElts :: PFState -> ([SEltLabel], LayerPos) -> PFCmd t
pfc_paste_to_newElts pfs (seltls, lp) = r where
  rid = pFState_maxID pfs + 1
  r = PFCNewElts ==> NE.fromList (zip3 [rid..] [lp..] seltls)

--pfc_duplicate_to_duplicate :: PFState -> [LayerPos] -> PFCmd t
--pfc_duplicate_to_duplicate pfs lps = r where
--  rids = map (Seq.index _pFState_layers) lps
--  r = PFCFDuplicate ==> rids
