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

import           Data.Aeson
import qualified Data.ByteString.Lazy          as LBS
import           Data.Dependent.Sum            ((==>))
import qualified Data.List.NonEmpty            as NE

import           Potato.Flow.Reflex.Cmd
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.Reflex.SEltLayers
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts


import           Control.Monad.Fix

-- loading new workspace stufff
type LoadFileEvent t =  Event t LBS.ByteString
type SetWSEvent t = Event t SEltTree

loadWSFromFile :: (Reflex t) => LoadFileEvent t -> SetWSEvent t
loadWSFromFile = fmapMaybe decode

data PFConfig t = PFConfig {
  --_pfc_setWorkspace :: SetWSEvent t
  _pfc_addElt       :: Event t (LayerPos, SEltLabel)
  , _pfc_removeElt  :: Event t LayerPos
  --, _pfc_moveElt    :: Event t (LayerEltId, LayerPos) -- new layer position (before or after removal?)
  --, _pfc_copy       :: Event t [LayerEltId]
  --, _pfc_paste      :: Event t ([SElt], LayerPos)
  --, _pfc_duplicate  :: Event t [LayerEltId]
  , _pfc_manipulate :: Event t ControllersWithId

  , _pfc_undo       :: Event t ()
  , _pfc_redo       :: Event t ()
}

data PFOutput t = PFOutput {
  _pfo_layers :: SEltLayerTree t
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
      $ sEltLayerTree_tagSuperSEltByPos layerTree _pfc_removeElt

  -- ACTION STACK
  let
    doActions = leftmostwarn "_actionStackConfig_do" [
        doAction_PFCNewElts
        , doAction_PFCDeleteElts
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
  -- * TODO modify elements :: ControllersWithId
  -- OUTPUTS
  -- * TODO modified elements :: PatchIntMap (Maybe SEltLabel, Maybe SEltLabel)
  let
    layerTreeConfig = SEltLayerTreeConfig {
        _sEltLayerTreeConfig_insert = leftmostwarn "_layerTreeConfig_add"
          [selectDo actionStack PFCNewElts, selectUndo actionStack PFCDeleteElts]
        , _sEltLayerTreeConfig_remove = leftmostwarn "_layerTreeConfig_remove"
          [selectUndo actionStack PFCNewElts, selectDo actionStack PFCDeleteElts]
      }
  layerTree :: SEltLayerTree t
    <- holdSEltLayerTree layerTreeConfig

  return $
    PFOutput {
      _pfo_layers = layerTree
    }
