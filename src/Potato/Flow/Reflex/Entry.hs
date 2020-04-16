{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Entry (

) where

import           Relude

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Data.Directory

import           Data.Aeson
import qualified Data.ByteString.Lazy           as LBS
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
  _pfc_setWorkspace :: SetWSEvent t
  , _pfc_addElt     :: Event t SEltLabel
  , _pfc_removeElt  :: Event t REltId
  , _pfc_moveElt    :: Event t (REltId, LayerPos) -- new layer position (before or after removal?)
  , _pfc_copy       :: Event t [REltId]
  , _pfc_paste      :: Event t ([SElt], LayerPos)
  , _pfc_duplicate  :: Event t [REltId]
}

data PFOutput t = PFOutput {
  _pfo_allElts     :: Behavior t (Map REltId (RElt t))

  -- or maybe just expose layer interface directly?
  , _pfo_layerView :: Dynamic t [REltId]
  , _pfo_changView :: Dynamic t (PatchMap REltId (RElt t)) -- only elements that were added, moved, or deleted
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
  actionStack <- holdActionStack actionStackConfig
{-
  data ActionStack t a = ActionStack {
    _actionStack_do            :: Event t a -- ^ fires when element is added to do stack
    , _actionStack_undo        :: Event t a -- ^ fires when element is added to undo stack
    , _actionStack_doneStack   :: Dynamic t [a] -- ^ stack of actions we've done
    , _actionStack_undoneStack :: Dynamic t [a] -- ^ stack of actions we've undone
  }
-}
  -- TODO prepared fanned outputs from ActionStack

  -- set up REltFactory and connect to DirectoryIdAssigner
  let
    rEltFactoryConfig = REltFactoryConfig {
        _rEltFactoryConfig_sEltTree = fmap (:[]) _pfc_addElt
      }
  rEltFactory <- holdREltFactory rEltFactoryConfig
  let
    directoryIdAssignerConfig = DirectoryIdAssignerConfig {
        _directoryIdAssignerConfig_assign = fmap NE.fromList (_rEltFactory_rEltTree rEltFactory)
      }
  directoryIdAssigner <- holdDirectoryIdAssigner directoryIdAssignerConfig



  -- set up Directory
  let
    directoryConfig = DirectoryConfig {
        -- TODO hook up to fanned outputs from actionStack
        _directoryMapConfig_add = never
        , _directoryMapConfig_remove = never
      }
  directory <- holdDirectory directoryConfig


  undefined
