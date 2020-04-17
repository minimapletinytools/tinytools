{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Entry (

) where

import           Relude

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Data.Directory
import           Reflex.Potato.Helpers

import           Data.Aeson
import qualified Data.ByteString.Lazy           as LBS
import           Data.Dependent.Sum             ((==>))
import qualified Data.List.NonEmpty             as NE

import           Potato.Flow.Reflex.Cmd
import           Potato.Flow.Reflex.Layers
import           Potato.Flow.Reflex.REltFactory
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts


import           Control.Monad.Fix

-- loading new workspace stufff
type LoadFileEvent t =  Event t LBS.ByteString
type SetWSEvent t = Event t SEltTree

loadWSFromFile :: (Reflex t) => LoadFileEvent t -> SetWSEvent t
loadWSFromFile = fmapMaybe decode

data PFConfig t = PFConfig {
  --_pfc_setWorkspace :: SetWSEvent t
  _pfc_addElt       :: Event t SEltLabel
  , _pfc_removeElt  :: Event t REltId
  --, _pfc_moveElt    :: Event t (REltId, LayerPos) -- new layer position (before or after removal?)
  --, _pfc_copy       :: Event t [REltId]
  --, _pfc_paste      :: Event t ([SElt], LayerPos)
  --, _pfc_duplicate  :: Event t [REltId]
  , _pfc_manipulate :: Event t ()
}

data PFOutput t = PFOutput {
  -- elements
  _pfo_allElts     :: Behavior t (Map REltId (REltLabel t))
  -- or maybe just expose layer interface directly?
  , _pfo_layerView :: Dynamic t [REltId]
  , _pfo_changView :: Dynamic t (PatchMap REltId (REltLabel t)) -- only elements that were added, moved, or deleted

  -- manipulators
}

holdPF ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => PFConfig t
  -> m (PFOutput t)
holdPF PFConfig {..} = mdo

  -- set up the action stack
  let
    actionStackConfig :: ActionStackConfig t (PFCmd t)
    actionStackConfig = ActionStackConfig {
      _actionStackConfig_do      = undefined
      , _actionStackConfig_undo  = undefined
      , _actionStackConfig_redo  = undefined
      , _actionStackConfig_clear = never
    }
  actionStack :: ActionStack t (PFCmd t)
    <- holdActionStack actionStackConfig

  -- set up DirectoryIdAssigner
  let
    rEltsCreatedEv = fmap NE.fromList (_rEltFactory_rEltTree rEltFactory)
    directoryIdAssignerConfig = DirectoryIdAssignerConfig {
        _directoryIdAssignerConfig_assign = fmap (:|[]) _pfc_addElt
      }
  directoryIdAssigner :: DirectoryIdAssigner t (SEltLabel)
    <- holdDirectoryIdAssigner directoryIdAssignerConfig

  -- TODO get rid of rEltFactory
  -- set up rEltFactory
  let
    rEltFactoryConfig = REltFactoryConfig {
        _rEltFactoryConfig_sEltTree = toList <$> _directoryIdAssigner_tag directoryIdAssigner _pfc_addElt
      }
    rEltFactory_action_newRElt :: Event t (PFCmd t)
    rEltFactory_action_newRElt = fmapMaybe (\x -> nonEmpty x >>= return . (PFCNewElts ==>)) $ _rEltFactory_rEltTree rEltFactory
  rEltFactory :: rEltFactory t
    <- holdREltFactory rEltFactoryConfig

  -- TODO map rEltsWithIdCreatedEv to CMD and send to ActionStack
  --PFCNewElts :: PFCmdTag t (NonEmpty (REltId, RElt t)) -- TODO needs LayerPos

  -- set up Directory
  let
    directoryConfig = DirectoryConfig {
        -- TODO hook up to fanned outputs from actionStack
        _directoryMapConfig_add = never
        , _directoryMapConfig_remove = never
      }
  directory :: Directory t (REltLabel t)
    <- holdDirectory directoryConfig

  -- set up LayerTree
  let
    ltc_add_do_PFCNewElts = layerTree_attachEndPos layerTree $ fmap toList $ selectDo actionStack PFCNewElts
    ltc_add_undo_PFCDeleteElt = fmap (\(i,e) -> (i,[e])) $ selectUndo actionStack PFCDeleteElt
    ltc_remove_undo_PFCNewElts = getId <<$>> selectUndo actionStack PFCNewElts
    ltc_remove_do_PFCDeleteElt = fmap (\(i,e) -> getId e :| []) $ selectDo actionStack PFCDeleteElt
    layerTreeConfig = LayerTreeConfig {
        _layerTreeConfig_add = leftmostwarn "_layerTreeConfig_add" [ltc_add_do_PFCNewElts, ltc_add_undo_PFCDeleteElt]
        , _layerTreeConfig_remove = leftmostwarn "_layerTreeConfig_remove" $ toList <<$>> [ltc_remove_undo_PFCNewElts, ltc_remove_do_PFCDeleteElt]
        , _layerTreeConfig_copy = never
      }
  layerTree :: LayerTree t (REltLabel t)
    <- holdLayerTree layerTreeConfig

  undefined
