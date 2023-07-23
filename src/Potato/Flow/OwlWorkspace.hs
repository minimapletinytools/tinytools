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

import           Potato.Flow.Llama
import           Potato.Flow.Math
import           Potato.Flow.Owl
import           Potato.Flow.OwlItem
import           Potato.Flow.OwlState
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception    (assert)
import qualified Data.IntMap.Strict   as IM
import qualified Data.IntSet          as IS
import qualified Data.Sequence        as Seq

-- TODO get rid of this, now needed
data OwlPFWorkspace = OwlPFWorkspace {
  _owlPFWorkspace_owlPFState    :: OwlPFState

  -- TODO move me elsewhere?
  , _owlPFWorkspace_llamaStack  :: LlamaStack
} deriving (Show, Generic)

instance NFData OwlPFWorkspace

loadOwlPFStateIntoWorkspace :: OwlPFState -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
loadOwlPFStateIntoWorkspace pfs ws = r where
  removeOld = fmap (const Nothing) (_owlTree_mapping . _owlPFState_owlTree . _owlPFWorkspace_owlPFState $ ws)
  addNew = IM.mapWithKey (\rid (oem,oe) -> Just (SuperOwl rid oem oe)) (_owlTree_mapping . _owlPFState_owlTree $ pfs)
  changes = IM.union addNew removeOld
  r = (OwlPFWorkspace pfs emptyLlamaStack, changes)

emptyWorkspace :: OwlPFWorkspace
emptyWorkspace = OwlPFWorkspace emptyOwlPFState emptyLlamaStack

-- UNTESTED
markWorkspaceSaved :: OwlPFWorkspace -> OwlPFWorkspace
markWorkspaceSaved pfw = r where
  as@LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  newas = as { _llamaStack_lastSaved = Just (length _llamaStack_done) }
  r = pfw { _owlPFWorkspace_llamaStack = newas }

undoWorkspace :: OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
undoWorkspace pfw =  r where
  LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  r = case _llamaStack_done of
    c : cs -> (OwlPFWorkspace newpfs (LlamaStack cs (undollama:_llamaStack_undone) _llamaStack_lastSaved), changes) where
      (newpfs, changes, undollama) = case _llama_apply c (_owlPFWorkspace_owlPFState pfw) of
        Left e  -> error $ show e
        Right x -> x
    _ -> (pfw, IM.empty)

redoWorkspace :: OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
redoWorkspace pfw = r where
  LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  r = case _llamaStack_undone of
    c : cs -> (OwlPFWorkspace newpfs (LlamaStack (dollama:_llamaStack_done) cs _llamaStack_lastSaved), changes) where
      (newpfs, changes, dollama) = case _llama_apply c (_owlPFWorkspace_owlPFState pfw) of
        Left e  -> error $ show e
        Right x -> x
    _ -> (pfw, IM.empty)

undoPermanentWorkspace :: OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
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
    c : cs -> (OwlPFWorkspace newpfs (LlamaStack cs _llamaStack_undone newLastSaved), changes) where
      (newpfs, changes, _) = case _llama_apply c (_owlPFWorkspace_owlPFState pfw) of
        Left e  -> error $ show e
        Right x -> x
    _ -> (pfw, IM.empty)

doLlamaWorkspace :: Llama -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doLlamaWorkspace llama pfw = r where
  oldpfs = _owlPFWorkspace_owlPFState pfw
  (newpfs, changes, mundollama) = case _llama_apply llama oldpfs of
    -- TODO would be nice to output error to user somehow?
    Left e  -> (oldpfs, IM.empty, Nothing)
    Right x -> case x of 
      (newpfs', changes', undollama') -> (newpfs', changes', Just undollama')
  LlamaStack {..} = (_owlPFWorkspace_llamaStack pfw)
  newLastSaved = case _llamaStack_lastSaved of
    Nothing -> Nothing
    Just x -> if length _llamaStack_done < x
      -- we "did" something when last save is still on undo stack, so we can never recover to last saved
      then Nothing
      -- we can still undo back to last save state
      else Just x
  r' = OwlPFWorkspace {
      _owlPFWorkspace_owlPFState       = newpfs
      , _owlPFWorkspace_llamaStack  = LlamaStack {
          _llamaStack_done = case mundollama of
            Nothing -> _llamaStack_done
            Just undollama -> undollama : _llamaStack_done
          , _llamaStack_undone = _llamaStack_undone
          , _llamaStack_lastSaved = newLastSaved
        }
    }
  r = (r', changes)

doLlamaWorkspaceUndoPermanentFirst :: Llama -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doLlamaWorkspaceUndoPermanentFirst llama ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  (undoedws, _) = undoPermanentWorkspace ws
  -- TODO do I need to combine changes from the undo operation? I think I do ;__;
  r = doLlamaWorkspace llama undoedws

doCmdWorkspace :: OwlPFCmd -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doCmdWorkspace cmd pfw = force r where
  r = doLlamaWorkspace (makePFCLlama cmd) pfw

doCmdOwlPFWorkspaceUndoPermanentFirst :: (OwlPFState -> OwlPFCmd) -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doCmdOwlPFWorkspaceUndoPermanentFirst cmdFn ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  (undoedws, _) = undoPermanentWorkspace ws
  undoedpfs = _owlPFWorkspace_owlPFState undoedws
  cmd = cmdFn undoedpfs
  -- TODO do I need to combine changes from the undo operation? I think I do ;__;
  r = doLlamaWorkspace (makePFCLlama cmd) undoedws

------ update functions via commands
data WSEvent =

  -- TODO DELETE these they will be replaced by Llama
  WSEAddElt (Bool, OwlSpot, OwlItem)
  | WSEAddTree (OwlSpot, MiniOwlTree)
  | WSEAddFolder (OwlSpot, Text)
  -- WIP
  | WSERemoveEltAndUpdateAttachments OwlParliament AttachmentMap
  -- | WSEDuplicate OwlParliament -- kiddos get duplicated??
  | WSELoad SPotatoFlow

  -- TODO get rid of undo first parameter 
  | WSEApplyLlama (Bool, Llama)


  | WSEUndo
  | WSERedo
  
  deriving (Show)
  

debugPrintBeforeAfterState :: (IsString a) => OwlPFState -> OwlPFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintOwlPFState stateBefore <> "\nAFTER: " <> debugPrintOwlPFState stateAfter


------ helpers for converting events to cmds
-- TODO assert elts are valid
pfc_addElt_to_newElts :: OwlPFState -> OwlSpot -> OwlItem -> OwlPFCmd
pfc_addElt_to_newElts pfs spot oelt = OwlPFCNewElts [(owlPFState_nextId pfs, spot, oelt)]

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

-- UNTESTED
makeLlamaToSetAttachedLinesToCurrentPosition :: OwlPFState -> AttachmentMap -> REltId -> [Llama]
makeLlamaToSetAttachedLinesToCurrentPosition pfs am target = case IM.lookup target am of
    Nothing       -> []
    Just attached -> fmap makeLlama . IS.toList $ attached
  where
    makeLlama :: REltId -> Llama
    makeLlama rid = case _superOwl_elt (hasOwlTree_mustFindSuperOwl pfs rid) of
        OwlItem _ (OwlSubItemLine sline) -> r where
          startAttachment = _sAutoLine_attachStart sline
          endAttachment = _sAutoLine_attachEnd sline
          affectstart = fmap _attachment_target startAttachment == Just target
          affectend = fmap _attachment_target endAttachment == Just target
          newstartpos = case maybeLookupAttachment False pfs startAttachment of
            Nothing -> error $ "expected to find attachment " <> show startAttachment
            Just x -> x
          newendpos = case maybeLookupAttachment False pfs endAttachment of
            Nothing -> error $ "expected to find attachment " <> show endAttachment
            Just x -> x
          newsline = sline {
              -- disconnect from target if it was deleted
              -- NOTE strictly speaking necessary! Not sure which way is better in multi-user mode
              _sAutoLine_attachStart = if affectstart then Nothing else _sAutoLine_attachStart sline
              , _sAutoLine_attachEnd = if affectend  then Nothing else _sAutoLine_attachEnd sline

              -- place endpoints in new place
              , _sAutoLine_start = if affectstart then newstartpos else _sAutoLine_start sline
              , _sAutoLine_end = if affectend then newendpos else _sAutoLine_end sline

            }
          r = makeSetLlama (rid, SEltLine newsline)
        _ -> error $ "found non-line element in attachment list"

noChanges :: OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
noChanges ws = (ws, IM.empty)

-- TODO rename to removeElts
removeEltAndUpdateAttachments_to_llama :: OwlPFState -> AttachmentMap -> OwlParliament -> Llama
removeEltAndUpdateAttachments_to_llama pfs am op@(OwlParliament rids) = r where
  removellama = makePFCLlama$  pfc_removeElt_to_deleteElts pfs op
  resetattachllamas = join $ fmap (makeLlamaToSetAttachedLinesToCurrentPosition pfs am) (toList rids)
  -- seems more correct to detach lines first and then delete the target so that undo operation is more sensible
  r = makeCompositionLlama $ resetattachllamas <> [removellama]

-- TODO take PotatoConfiguration here???
updateOwlPFWorkspace :: WSEvent -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
updateOwlPFWorkspace evt ws = let
  lastState = _owlPFWorkspace_owlPFState ws
  r = case evt of

    -- TODO DELETE ALL OF THESE
    WSEAddElt (undo, spot, oelt) -> if undo
      then doCmdOwlPFWorkspaceUndoPermanentFirst (\pfs -> pfc_addElt_to_newElts pfs spot oelt) ws
      else doCmdWorkspace (pfc_addElt_to_newElts lastState spot oelt) ws
    WSEAddTree x -> doCmdWorkspace (OwlPFCNewTree (swap x)) ws
    WSEAddFolder x -> doCmdWorkspace (pfc_addFolder_to_newElts lastState x) ws
    WSERemoveEltAndUpdateAttachments x am -> doLlamaWorkspace (removeEltAndUpdateAttachments_to_llama lastState am x) ws

    -- TODO add validation step (in particular, we need this for canvas size)
    WSEApplyLlama (undo, x) -> if undo
      then doLlamaWorkspaceUndoPermanentFirst x ws
      else doLlamaWorkspace x ws
    
    WSEUndo -> undoWorkspace ws
    WSERedo -> redoWorkspace ws
    WSELoad x -> loadOwlPFStateIntoWorkspace (sPotatoFlow_to_owlPFState x) ws
  afterState = _owlPFWorkspace_owlPFState (fst r)
  isValidAfter = owlPFState_isValid afterState
  in if isValidAfter
    then r
    else error ("INVALID " <> show evt <> "\n" <> debugPrintBeforeAfterState lastState afterState)