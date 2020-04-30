{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Entry (
  PFConfig(..)
  , PFOutput(..)
  , holdPF
) where

import           Relude

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Data.Directory
import           Reflex.Potato.Helpers

import           Data.Dependent.Sum            ((==>))
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe                    (fromJust)
import qualified Data.Sequence                 as Seq
import           Data.Tuple.Extra

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Canvas
import           Potato.Flow.Reflex.Cmd
import           Potato.Flow.Reflex.SEltLayers
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Lens                  (over, _1, _2, _3)
import           Control.Monad.Fix

-- loading new workspace stufff
{-
type LoadFileEvent t =  Event t LBS.ByteString
type SetWSEvent t = Event t SEltTree
loadWSFromFile :: (Reflex t) => LoadFileEvent t -> SetWSEvent t
loadWSFromFile = fmapMaybe decode
-}

data PFConfig t = PFConfig {
  --_pfc_setWorkspace :: SetWSEvent t
  _pfc_addElt       :: Event t (LayerPos, SEltLabel)
  -- TODO take a selection
  , _pfc_removeElt  :: Event t LayerPos
  --, _pfc_moveElt    :: Event t (LayerEltId, LayerPos) -- new layer position (before or after removal?)
  --, _pfc_copy       :: Event t [LayerEltId]
  --, _pfc_paste      :: Event t ([SElt], LayerPos)
  --, _pfc_duplicate  :: Event t [LayerEltId]
  , _pfc_manipulate :: Event t ControllersWithId

  , _pfc_undo       :: Event t ()
  , _pfc_redo       :: Event t ()

  , _pfc_save       :: Event t ()
}

data PFOutput t = PFOutput {
  _pfo_layers           :: SEltLayerTree t
  , _pfo_canvas         :: Canvas t
  , _pfo_saved          :: Event t SPotatoFlow

  -- for debugging and temp rendering, to be removed once incremental rendering is done
  , _pfo_potato_state   :: Behavior t [SuperSEltLabel]
  , _pfo_potato_changed :: Event t ()
}

holdPF ::
  forall t m. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m)
  => PFConfig t
  -> m (PFOutput t)
holdPF PFConfig {..} = mdo
  -- PREP ACTIONS
  -- TODO
  --_pfc_addElt       :: Event t (LayerPos, SEltLabel)
  --, _pfc_removeElt  :: Event t LayerEltId
  --, _pfc_manipulate :: Event t ControllersWithId

  let
    doAction_PFCDeleteElts :: Event t (PFCmd t)
    doAction_PFCDeleteElts =
      fmap (PFCDeleteElts ==>)
      $ fmap (:|[])
      -- PARTIAL
      $ fmap fromJust
      $ pushAlways (sEltLayerTree_sampleSuperSEltByPos layerTree) _pfc_removeElt

    doAction_PFCManipulate :: Event t (PFCmd t)
    doAction_PFCManipulate = fmap (PFCManipulate ==>) _pfc_manipulate

  -- ACTION STACK
  let
    doActions = leftmostwarn "_actionStackConfig_do" [
        doAction_PFCNewElts
        , doAction_PFCDeleteElts
        , doAction_PFCManipulate
      ]
    actionStackConfig :: ActionStackConfig t (PFCmd t)
    actionStackConfig = ActionStackConfig {
      -- TODO do proper event tracing
      --_actionStackConfig_do      = traceEventWith  (const "DO: ") doActions
      _actionStackConfig_do      = doActions
      , _actionStackConfig_undo  = _pfc_undo
      , _actionStackConfig_redo  = _pfc_redo
      , _actionStackConfig_clear = never
    }
  actionStack :: ActionStack t (PFCmd t)
    <- holdActionStack actionStackConfig

  -- DIRECTORY ID ASSIGNER
  -- INPUTS:
  -- * _pfc_addElt :: Event t SEltLabel
  -- * TODO _pfc_paste
  -- * TODO _pfc_duplicate
  -- OUTPUTS:
  -- * doAction_PFCNewElts :: Event t (NonEmpty SuperSEltLabel)
  -- * TODO doAction_paste/duplicate

  let
    directoryIdAssignerConfig = DirectoryIdAssignerConfig {
        _directoryIdAssignerConfig_assign = fmap (:|[]) $ _pfc_addElt
      }
  directoryIdAssigner :: DirectoryIdAssigner t (LayerPos, SEltLabel)
    <- holdDirectoryIdAssigner directoryIdAssignerConfig
  let
    flattenTuple212 (a,(b,c)) = (a,b,c)
    doAction_PFCNewElts :: Event t (PFCmd t)
    doAction_PFCNewElts =
      fmap (PFCNewElts ==>)
      $ flattenTuple212
      <<$>> _directoryIdAssigner_tag directoryIdAssigner _pfc_addElt


  -- SELTLAYERS
  -- INPUTS
  -- * selectDo/Undo actionStack PFCNewElts :: Event t (NonEmpty SuperSEltLabel)
  -- * selectDo/Undo actionStack PFCDeleteElts :: Event t (NonEmpty SuperSEltLabel)
  -- * selectDo/Undo actionStack PFCManipulate :: ControllersWithId
  -- OUTPUTS
  -- * SEltLayerTree
  let
    layerTreeConfig = SEltLayerTreeConfig {
        _sEltLayerTreeConfig_insert = leftmostwarn "_layerTreeConfig_add"
          [selectDo actionStack PFCNewElts, selectUndo actionStack PFCDeleteElts]
        , _sEltLayerTreeConfig_remove = leftmostwarn "_layerTreeConfig_remove"
          [selectUndo actionStack PFCNewElts, selectDo actionStack PFCDeleteElts]
        , _sEltLayerTree_directory_doManipulate = selectDo actionStack PFCManipulate
        , _sEltLayerTree_directory_undoManipulate = selectUndo actionStack PFCManipulate
      }
  layerTree :: SEltLayerTree t
    <- holdSEltLayerTree layerTreeConfig

  -- CANVAS
  -- INPUTS
  -- * selectDo/Undo actionStack PFCResizeCanvas :: Event t DeltaLBox
  -- OUTPUTS
  -- * Canvas
  let
    canvasConfig = CanvasConfig {
        _canvasConfig_resize = leftmost
          [fmap (flip plusDelta) (selectDo actionStack PFCResizeCanvas)
          , fmap (flip minusDelta) (selectDo actionStack PFCResizeCanvas)]
      }
  canvas <- holdCanvas canvasConfig

  -- PREP PFO
  let
    pushStateFn :: (MonadSample t m') => () -> m' (SCanvas, [SuperSEltLabel])
    pushStateFn _ = do
      layers <- sample . current $ _sEltLayerTree_view layerTree
      contents <- sample $ _directory_contents $ _sEltLayerTree_directory layerTree
      scanvas <- sample (current $ _canvas_box canvas) >>= return . SCanvas
      let
        -- PARTIAL
        foldfn index rid acc = (rid, index, (contents IM.! rid)) : acc
      return $ (scanvas, Seq.foldrWithIndex foldfn [] layers)

  return $
    PFOutput {
      _pfo_layers = layerTree
      , _pfo_saved = fmap (uncurry SPotatoFlow)
        $ fmap (over _2 (fmap thd3))
        $ pushAlways pushStateFn _pfc_save
      , _pfo_potato_state = fmap snd $ pull (pushStateFn ())
      , _pfo_potato_changed = void $ leftmost [_actionStack_do actionStack, _actionStack_undo actionStack]
    }
