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
import Potato.Flow.Preview

import           Control.Exception    (assert)
import qualified Data.IntMap.Strict   as IM
import qualified Data.IntSet          as IS
import qualified Data.Sequence        as Seq

-- TODO get rid of this, now needed
data OwlPFWorkspace = OwlPFWorkspace {
  _owlPFWorkspace_owlPFState    :: OwlPFState

  -- TODO rename to localLlamaStack
  , _owlPFWorkspace_llamaStack  :: LlamaStack

  -- WIP preview stuff
  -- Llama is the undo Llama for the preview as the preview has already been applied to _owlPFWorkspace_owlPFState
  , _owlPFWorkspace_localPreview :: Maybe (Shepard, Shift, Llama) 
  , _owlPFWorkspace_remotePreviews :: [(Shepard, Shift, Llama)]

} deriving (Show, Generic)

instance NFData OwlPFWorkspace

owlPFWorkspace_hasLocalPreview :: OwlPFWorkspace -> Bool
owlPFWorkspace_hasLocalPreview pfw = isJust (_owlPFWorkspace_localPreview pfw)

-- NOTE this will reset all previews and the LlamaStack, be sure to synchronize with your ordering service!!!
loadOwlPFStateIntoWorkspace :: OwlPFState -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
loadOwlPFStateIntoWorkspace pfs ws = (next_ws, changes) where
  removeOld = fmap (const Nothing) (_owlTree_mapping . _owlPFState_owlTree . _owlPFWorkspace_owlPFState $ ws)
  addNew = IM.mapWithKey (\rid (oem,oe) -> Just (SuperOwl rid oem oe)) (_owlTree_mapping . _owlPFState_owlTree $ pfs)
  changes = IM.union addNew removeOld
  next_ws = emptyWorkspace {
      _owlPFWorkspace_owlPFState = pfs
      , _owlPFWorkspace_llamaStack = emptyLlamaStack
    }

emptyWorkspace :: OwlPFWorkspace
emptyWorkspace =  OwlPFWorkspace {
    _owlPFWorkspace_owlPFState    = emptyOwlPFState
    , _owlPFWorkspace_llamaStack  = emptyLlamaStack
    , _owlPFWorkspace_localPreview = Nothing
    , _owlPFWorkspace_remotePreviews = []
  }

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
    c : cs -> (next_ws , changes) where
      (newpfs, changes, undollama) = case _llama_apply c (_owlPFWorkspace_owlPFState pfw) of
        Left e  -> error $ show e
        Right x -> x
      next_ws =  pfw {
          _owlPFWorkspace_owlPFState = newpfs
          , _owlPFWorkspace_llamaStack = (LlamaStack cs (undollama:_llamaStack_undone) _llamaStack_lastSaved)
        }
    _ -> (pfw, IM.empty)

redoWorkspace :: OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
redoWorkspace pfw = r where
  LlamaStack {..} = _owlPFWorkspace_llamaStack pfw
  r = case _llamaStack_undone of
    c : cs -> (next_ws, changes) where
      (newpfs, changes, dollama) = case _llama_apply c (_owlPFWorkspace_owlPFState pfw) of
        Left e  -> error $ show e
        Right x -> x
      next_ws = pfw {
        _owlPFWorkspace_owlPFState = newpfs
        , _owlPFWorkspace_llamaStack = (LlamaStack (dollama:_llamaStack_done) cs _llamaStack_lastSaved)
      }
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
    c : cs -> (next_ws, changes) where
      (newpfs, changes, _) = case _llama_apply c (_owlPFWorkspace_owlPFState pfw) of
        Left e  -> error $ show e
        Right x -> x
      next_ws =  pfw {
        _owlPFWorkspace_owlPFState = newpfs
        , _owlPFWorkspace_llamaStack = (LlamaStack cs _llamaStack_undone newLastSaved)
      }
    _ -> (pfw, IM.empty)



moveLlamaStackDone :: Llama -> LlamaStack -> LlamaStack
moveLlamaStackDone undollama LlamaStack {..} = r where
  newLastSaved = case _llamaStack_lastSaved of
    Nothing -> Nothing
    Just x -> if length _llamaStack_done < x
      -- we "did" a change after last save
      then Just x
      -- we "did" a change from last save
      else Nothing
  r = LlamaStack {
      _llamaStack_done = undollama : _llamaStack_done
      , _llamaStack_undone = _llamaStack_undone
      , _llamaStack_lastSaved = newLastSaved
    }

doLlamaWorkspace :: Llama -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doLlamaWorkspace = doLlamaWorkspace' True


doLlamaWorkspace' :: Bool -> Llama -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doLlamaWorkspace' updatestack llama pfw = r where
  oldpfs = _owlPFWorkspace_owlPFState pfw
  (newpfs, changes, mundollama) = case _llama_apply llama oldpfs of
    -- TODO would be nice to output error to user somehow?
    Left e  -> case e of
      ApplyLlamaError_Fatal x -> error x
      ApplyLLamaError_Soft _ -> (oldpfs, IM.empty, Nothing)
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
      , _owlPFWorkspace_llamaStack  = if not updatestack then _owlPFWorkspace_llamaStack pfw  else LlamaStack {
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
  (undoedws, undochanges) = undoPermanentWorkspace ws
  (newpfs, changes) = doLlamaWorkspace llama undoedws
  r = (newpfs, IM.union changes undochanges)

doCmdWorkspace :: OwlPFCmd -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doCmdWorkspace cmd pfw = force r where
  r = doLlamaWorkspace (makePFCLlama cmd) pfw

doCmdOwlPFWorkspaceUndoPermanentFirst :: (OwlPFState -> OwlPFCmd) -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
doCmdOwlPFWorkspaceUndoPermanentFirst cmdFn ws = r where
  -- undoPermanent is actually not necessary as the next action clears the redo stack anyways
  (undoedws, undochanges) = undoPermanentWorkspace ws
  undoedpfs = _owlPFWorkspace_owlPFState undoedws
  cmd = cmdFn undoedpfs
  (newpfs, changes) = doLlamaWorkspace (makePFCLlama cmd) undoedws
  r = (newpfs, IM.union changes undochanges)


------ update functions via commands
data WSEvent =
  -- TODO DELETE
  -- TODO get rid of undo first parameter 
  WSEApplyLlama (Bool, Llama)

  | WSEApplyPreview Shepard Shift Preview

  | WSEUndo
  | WSERedo
  | WSELoad SPotatoFlow
  deriving (Show)
  

debugPrintBeforeAfterState :: (IsString a) => OwlPFState -> OwlPFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintOwlPFState stateBefore <> "\nAFTER: " <> debugPrintOwlPFState stateAfter


noChanges :: OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
noChanges ws = (ws, IM.empty)

clearLocalPreview :: (OwlPFWorkspace, SuperOwlChanges) -> (OwlPFWorkspace, SuperOwlChanges)
clearLocalPreview (ws, changes) = (ws { _owlPFWorkspace_localPreview = Nothing }, changes)

moveLocalPreviewToLlamaStackAndClear :: OwlPFWorkspace -> OwlPFWorkspace
moveLocalPreviewToLlamaStackAndClear ws = case _owlPFWorkspace_localPreview ws of
  Nothing -> ws
  Just (shep, shift, undollama) -> r_1 where
    newstack = moveLlamaStackDone undollama (_owlPFWorkspace_llamaStack ws)
    r_1 = ws { 
        _owlPFWorkspace_llamaStack = newstack 
        , _owlPFWorkspace_localPreview = Nothing
      }
    


-- TODO take PotatoConfiguration here???
updateOwlPFWorkspace :: WSEvent -> OwlPFWorkspace -> (OwlPFWorkspace, SuperOwlChanges)
updateOwlPFWorkspace evt ws = let
  lastState = _owlPFWorkspace_owlPFState ws
  r = case evt of

    WSEApplyPreview shep shift preview -> case preview of
      Preview op llama -> case op of
        PO_Start -> doLlamaWorkspace llama ws
        PO_Continue -> doLlamaWorkspace llama ws
        PO_StartAndCommit -> doLlamaWorkspace llama ws
        PO_ContinueAndCommit -> doLlamaWorkspace llama ws
      Preview_Commit -> clearLocalPreview $ noChanges ws
      Preview_Cancel -> case _owlPFWorkspace_localPreview ws of 
        Nothing -> error "expected local preview"
        Just (_, _, undollama) -> clearLocalPreview $ doLlamaWorkspace' False undollama ws

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