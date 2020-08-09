{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.New.Entry (
  PFConfig(..)
  , PFOutput(..)
  , holdPF
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Data.Dependent.Sum        ((==>))
import qualified Data.IntMap.Strict        as IM
import qualified Data.Sequence             as Seq

import           Potato.Flow.Math
import           Potato.Flow.New.Cmd
import           Potato.Flow.New.State
import           Potato.Flow.New.Workspace
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Monad.Fix

-- loading new workspace stufff
{-
type LoadFileEvent t =  Event t LBS.ByteString
type SetWSEvent t = Event t SEltTree
loadWSFromFile :: (Reflex t) => LoadFileEvent t -> SetWSEvent t
loadWSFromFile = fmapMaybe decode
-}

-- _pfc_([^\s]+)\s+:: Event t\s.*
data PFConfig t = PFConfig {
  --_pfc_setWorkspace :: SetWSEvent t
  -- | don't use this to add folders, as they need to be added with matching pair to ensure scoping property
  _pfc_addElt         :: Event t (LayerPos, SEltLabel)
  , _pfc_addFolder    :: Event t (LayerPos, Text)
  , _pfc_deleteElts   :: Event t [LayerPos]
  , _pfc_moveElt      :: Event t ([LayerPos], LayerPos) -- new layer position is before removal
  , _pfc_copy         :: Event t [LayerPos]
  , _pfc_paste        :: Event t LayerPos
  --, _pfc_duplicate    :: Event t [LayerPos]
  , _pfc_manipulate   :: Event t ControllersWithId
  , _pfc_resizeCanvas :: Event t DeltaLBox
  , _pfc_undo         :: Event t ()
  , _pfc_redo         :: Event t ()
  , _pfc_load         :: Event t SPotatoFlow
  , _pfc_save         :: Event t ()
}

data PFOutput t = PFOutput {
  _pfo_pFState          :: Dynamic t (PFState)
  , _pfo_loaded         :: Event t ()
  , _pfo_saved          :: Event t SPotatoFlow
  , _pfo_potato_changed :: Event t SEltLabelChanges
}

data PFTotalState = PFTotalState {
  _pFTotalState_workspace   :: PFWorkspace
  , _pFTotalState_clipboard :: [SEltLabel]
}

debugPrintBeforeAfterState :: (IsString a) => PFState -> PFState -> a
debugPrintBeforeAfterState stateBefore stateAfter = fromString $ "BEFORE: " <> debugPrintPFState stateBefore <> "\nAFTER: " <> debugPrintPFState stateAfter

doCmdPFTotalState :: PFCmd -> PFTotalState -> PFTotalState
doCmdPFTotalState cmd pfts = r where
  r = pfts { _pFTotalState_workspace = doCmdWorkspace cmd (_pFTotalState_workspace pfts) }
  --stateBefore = (_pFWorkspace_state (_pFTotalState_workspace pfts))
  --isValidBefore = pFState_isValid stateBefore
  --stateAfter = (_pFWorkspace_state (_pFTotalState_workspace r))
  --isValidAfter = pFState_isValid stateAfter
  --trace (show cmd <> "\n" <> debugPrintBeforeAfterState stateBefore stateAfter) r'

doCmdPFTTotalStateUndoFirst :: PFCmd -> PFTotalState -> PFTotalState
doCmdPFTTotalStateUndoFirst cmd pfts = pfts { _pFTotalState_workspace = doCmdWorkspaceUndoFirst cmd (_pFTotalState_workspace pfts) }


data PFEventTag =
  -- TODO add undo first param
  PFEAddElt (LayerPos, SEltLabel)
  | PFEAddFolder (LayerPos, Text)
  | PFERemoveElt [LayerPos]
  | PFEMoveElt ([LayerPos], LayerPos)
  | PFECopy [LayerPos]
  | PFEPaste LayerPos
  -- | PFEDuplicate [LayerPos]
  -- TODO add undo first param
  | PFEManipulate ControllersWithId
  | PFEResizeCanvas DeltaLBox
  | PFEUndo
  | PFERedo
  | PFELoad SPotatoFlow
  deriving (Show)


holdPF ::
  forall t m. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m)
  => PFConfig t
  -> m (PFOutput t)
holdPF PFConfig {..} = mdo
  let
    pfevent = leftmostWarn "PFConfig"
      [ PFEAddElt <$> _pfc_addElt
      , PFEAddFolder <$> _pfc_addFolder
      , PFERemoveElt <$> _pfc_deleteElts
      , PFEMoveElt <$> _pfc_moveElt
      -- , PFEDuplicate <$> _pfc_duplicate
      , PFEManipulate <$> _pfc_manipulate
      , PFEResizeCanvas <$> _pfc_resizeCanvas
      , PFECopy <$> _pfc_copy
      , PFEPaste <$> _pfc_paste
      , PFEUndo <$ _pfc_undo
      , PFERedo <$ _pfc_redo
      , PFELoad <$> _pfc_load ]

    foldfn :: PFEventTag -> PFTotalState -> PFTotalState
    foldfn evt pfts = let
      lastState = _pFWorkspace_state $ _pFTotalState_workspace pfts
      r = case evt of
        PFEAddElt x -> doCmdPFTotalState (pfc_addElt_to_newElts lastState x) pfts
        PFEAddFolder x -> doCmdPFTotalState (pfc_addFolder_to_newElts lastState x) pfts
        PFERemoveElt x -> doCmdPFTotalState (pfc_removeElt_to_deleteElts lastState x) pfts
        PFEManipulate x -> doCmdPFTotalState (PFCManipulate ==> x) pfts
        PFEMoveElt x -> doCmdPFTotalState (PFCMove ==> x) pfts
        PFEResizeCanvas x -> doCmdPFTotalState (PFCResizeCanvas ==> x) pfts
        PFEPaste x -> doCmdPFTotalState (pfc_paste_to_newElts lastState (_pFTotalState_clipboard pfts, x)) pfts
        PFECopy x -> pfts { _pFTotalState_clipboard = pFState_copyElts (_pFWorkspace_state (_pFTotalState_workspace pfts)) x }
        PFEUndo -> pfts { _pFTotalState_workspace = undoWorkspace (_pFTotalState_workspace pfts) }
        PFERedo -> pfts { _pFTotalState_workspace = redoWorkspace (_pFTotalState_workspace pfts) }
        PFELoad x -> pfts {
          _pFTotalState_workspace = (_pFTotalState_workspace pfts) {
            _pFWorkspace_state = sPotatoFlow_to_pFState x
            , _pFWorkspace_actionStack = emptyActionStack }
          }
      afterState = (_pFWorkspace_state $ _pFTotalState_workspace r)
      isValidAfter = pFState_isValid afterState
      in
        if isValidAfter then r else
          error ("INVALID " <> show evt <> "\n" <> debugPrintBeforeAfterState lastState afterState)

  pfTotalState <- foldDyn foldfn (PFTotalState emptyWorkspace []) pfevent

  let
    savepushfn _ = do
      pfts <- sample . current $ pfTotalState
      return $ pFState_to_sPotatoFlow $ _pFWorkspace_state (_pFTotalState_workspace pfts)
    r_saved = pushAlways savepushfn _pfc_save
    r_state = fmap (_pFWorkspace_state . _pFTotalState_workspace) pfTotalState
    r_changes = fmap (_pFWorkspace_lastChanges . _pFTotalState_workspace) pfTotalState

  return PFOutput {
      _pfo_pFState = r_state
      , _pfo_saved              = r_saved -- :: Event t SPotatoFlow
      , _pfo_loaded = void _pfc_load
      -- TOOD remove 'pfevent $> IM.empty' once load/save is done properly
      -- probably just need to address TODO in workspaceFromState
      , _pfo_potato_changed     = leftmost [updated r_changes, pfevent $> IM.empty]
    }
