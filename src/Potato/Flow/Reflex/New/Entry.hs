{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.New.Entry (
  PotatoTotal(..)
  , potato_simplifyPotatoTotal

  , PFConfig(..)
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
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (fromJust)
import qualified Data.Sequence                 as Seq
import           Data.Tuple.Extra

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Canvas
import           Potato.Flow.Reflex.Cmd
import           Potato.Flow.Reflex.SEltLayers
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Lens                  (over, _2)
import           Control.Monad.Fix

-- loading new workspace stufff
{-
type LoadFileEvent t =  Event t LBS.ByteString
type SetWSEvent t = Event t SEltTree
loadWSFromFile :: (Reflex t) => LoadFileEvent t -> SetWSEvent t
loadWSFromFile = fmapMaybe decode
-}

-- | temp (or maybe not temp) way to track all changes in SEltLayerTree
-- TBH not so potatoes, this is legit
data PotatoTotal = PotatoTotal {
  -- map of REltId to SEltLabel
  _potatoTotal_sEltLabelMap  :: IM.IntMap SEltLabel
  -- map of REltId to LayerPos
  --, _potatoTotal_layerPosMap :: IM.IntMap LayerPos
  , _potatoTotal_layerPosMap :: REltId -> Maybe LayerPos
  , _potatoTotal_layers      :: Seq REltId
}

-- this is potato
potato_simplifyPotatoTotal :: PotatoTotal -> [SuperSEltLabel]
potato_simplifyPotatoTotal PotatoTotal {..} = r where
  foldfn index rid acc = (rid, index, (_potatoTotal_sEltLabelMap IM.! rid)) : acc
  r = Seq.foldrWithIndex foldfn [] _potatoTotal_layers



data PFConfig t = PFConfig {
  --_pfc_setWorkspace :: SetWSEvent t
  -- | don't use this to add folders, as they need to be added with matching pair to ensure scoping property
  _pfc_addElt         :: Event t (LayerPos, SEltLabel)
  , _pfc_addFolder    :: Event t (LayerPos, Text)
  -- TODO take a selection
  , _pfc_removeElt    :: Event t LayerPos
  --, _pfc_moveElt    :: Event t (LayerEltId, LayerPos) -- new layer position (before or after removal?)
  --, _pfc_copy       :: Event t [LayerEltId]
  --, _pfc_paste      :: Event t ([SElt], LayerPos)
  --, _pfc_duplicate  :: Event t [LayerEltId]
  , _pfc_manipulate   :: Event t ControllersWithId
  , _pfc_resizeCanvas :: Event t DeltaLBox

  , _pfc_undo         :: Event t ()
  , _pfc_redo         :: Event t ()

  , _pfc_load         :: Event t SPotatoFlow
  , _pfc_save         :: Event t ()
}

data PFOutput t = PFOutput {

  -- TODO rename to _pfo_sEltLayerTree
  _pfo_layers               :: SEltLayerTree t
  , _pfo_canvas             :: Canvas t

  -- TODO
  --, _pfo_loaded :: Event t ()

  , _pfo_saved              :: Event t SPotatoFlow

  -- for debugging and temp rendering, to be removed once incremental rendering is done
  , _pfo_potato_changed     :: Event t ()
  -- temp access to entire state, or maybe not so temp
  , _pfo_potato_potatoTotal :: Dynamic t PotatoTotal
}

holdPF ::
  forall t m. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m)
  => PFConfig t
  -> m (PFOutput t)
holdPF PFConfig {..} = mdo
  undefined
