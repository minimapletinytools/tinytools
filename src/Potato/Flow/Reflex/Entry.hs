-- TODO rename to PF
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Entry (
  PFConfig(..)
  , neverPFConfig
  , PFOutput(..)
  , PFEventTag(..)
  , holdPF
  , holdPFWithInitialState
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Data.Dependent.Sum    ((==>))
import qualified Data.IntMap.Strict    as IM
import qualified Data.Sequence         as Seq

import           Potato.Flow.Cmd
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types
import           Potato.Flow.Workspace

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
  _pfc_addElt         :: Event t (Bool, (LayerPos, SEltLabel))
  , _pfc_addFolder    :: Event t (LayerPos, Text)
  , _pfc_deleteElts   :: Event t [LayerPos]
  , _pfc_moveElt      :: Event t ([LayerPos], LayerPos) -- new layer position is before removal
  , _pfc_copy         :: Event t [LayerPos]
  , _pfc_paste        :: Event t LayerPos
  --, _pfc_duplicate    :: Event t [LayerPos]
  , _pfc_manipulate   :: Event t (Bool, ControllersWithId)
  , _pfc_resizeCanvas :: Event t DeltaLBox
  , _pfc_undo         :: Event t ()
  , _pfc_redo         :: Event t ()
  , _pfc_load         :: Event t SPotatoFlow
  , _pfc_save         :: Event t ()
}

neverPFConfig :: (Reflex t) => PFConfig t
neverPFConfig = PFConfig {
  _pfc_addElt         = never
  , _pfc_addFolder    = never
  , _pfc_deleteElts   = never
  , _pfc_moveElt      = never
  , _pfc_copy         = never
  , _pfc_paste        = never
  --, _pfc_duplicate    = never
  , _pfc_manipulate   = never
  , _pfc_resizeCanvas = never
  , _pfc_undo         = never
  , _pfc_redo         = never
  , _pfc_load         = never
  , _pfc_save         = never
}


-- TODO reanme to PF
data PFOutput t = PFOutput {
  _pfo_pFState             :: Dynamic t (PFState)

  -- granular access to individual parts of PFState
  -- does it make sense to group together layers and directory?
  , _pfo_pFState_layers    :: Dynamic t (Seq REltId)
  , _pfo_pFState_directory :: Dynamic t (REltIdMap SEltLabel)
  , _pfo_pFState_canvas    :: Dynamic t (SCanvas)

  -- takes REltId to LayerPos
  , _pfo_layerPosMap       :: Dynamic t (REltIdMap LayerPos)

  , _pfo_potato_changed    :: Event t SEltLabelChangesWithLayerPos


  , _pfo_loaded            :: Event t ()
  , _pfo_saved             :: Event t SPotatoFlow
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
  PFEAddElt (Bool, (LayerPos, SEltLabel))
  | PFEAddFolder (LayerPos, Text)
  | PFERemoveElt [LayerPos]
  | PFEMoveElt ([LayerPos], LayerPos)
  | PFECopy [LayerPos]
  | PFEPaste LayerPos
  -- | PFEDuplicate [LayerPos]
  | PFEManipulate (Bool, ControllersWithId)
  | PFEResizeCanvas DeltaLBox
  | PFEUndo
  | PFERedo
  | PFELoad SPotatoFlow
  deriving (Show)

holdPF ::
  forall t m. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m)
  => PFConfig t
  -> m (PFOutput t)
holdPF pfc = holdPFWithInitialState emptyPFState pfc

-- intended for testing, though maybe also useful for loading via command line
holdPFWithInitialState ::
  forall t m. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m)
  => PFState
  -> PFConfig t
  -> m (PFOutput t)
holdPFWithInitialState initialState PFConfig {..} = mdo
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
        PFEAddElt (undo, x) -> if undo
          then doCmdPFTTotalStateUndoFirst (pfc_addElt_to_newElts lastState x) pfts
          else doCmdPFTotalState (pfc_addElt_to_newElts lastState x) pfts
        PFEAddFolder x -> doCmdPFTotalState (pfc_addFolder_to_newElts lastState x) pfts
        PFERemoveElt x -> doCmdPFTotalState (pfc_removeElt_to_deleteElts lastState x) pfts
        PFEManipulate (undo, x) -> if undo
          then doCmdPFTTotalStateUndoFirst (PFCManipulate ==> x) pfts
          else doCmdPFTotalState (PFCManipulate ==> x) pfts
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

  pfTotalStateDyn <- foldDyn foldfn (PFTotalState (workspaceFromState initialState) []) pfevent

  let
    savepushfn _ = do
      pfts <- sample . current $ pfTotalStateDyn
      return $ pFState_to_sPotatoFlow $ _pFWorkspace_state (_pFTotalState_workspace pfts)
    r_saved = pushAlways savepushfn _pfc_save
    r_state = fmap (_pFWorkspace_state . _pFTotalState_workspace) pfTotalStateDyn

  -- pull stuff uniquely out of state/workspace
  --TODO use https://hackage.haskell.org/package/reflex-0.7.1.0/docs/Reflex-Dynamic-Uniq.html do I just uniqDynamic . fromUniqDynamic ?
  r_changes' <- holdUniqDyn $ fmap (_pFWorkspace_lastChanges . _pFTotalState_workspace) pfTotalStateDyn
  r_layers <- holdUniqDyn $ fmap (_pFState_layers . _pFWorkspace_state .  _pFTotalState_workspace) pfTotalStateDyn
  r_directory <- holdUniqDyn $ fmap (_pFState_directory . _pFWorkspace_state .  _pFTotalState_workspace) pfTotalStateDyn
  r_canvas <- holdUniqDyn $ fmap (_pFState_canvas . _pFWorkspace_state .  _pFTotalState_workspace) pfTotalStateDyn

  let
    r_layerPosMap = fmap (Seq.foldrWithIndex (\lp rid acc -> IM.insert rid lp acc) IM.empty) (r_layers)
    r_changes = ffor2 r_changes' r_layerPosMap $ \changes layerPosMap ->
      IM.mapWithKey (\rid v -> fmap (\seltl -> (layerPosMap IM.! rid, seltl)) v) changes

  return PFOutput {
      _pfo_pFState = r_state

      , _pfo_pFState_layers    = r_layers
      , _pfo_pFState_directory = r_directory
      , _pfo_pFState_canvas    = r_canvas

      , _pfo_layerPosMap = r_layerPosMap

      -- TOOD remove 'pfevent $> IM.empty' once load/save is done properly
      -- probably just need to address TODO in workspaceFromState
      , _pfo_potato_changed     = leftmost [updated r_changes, pfevent $> IM.empty]

      , _pfo_saved              = r_saved -- :: Event t SPotatoFlow
      , _pfo_loaded = void _pfc_load
    }
