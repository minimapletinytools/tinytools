module Potato.Flow.Reflex.Types (
  LayerPos
  , REltId
  , REltIdMap
  , PatchREltIdMap
  , SuperSEltLabel
  , ControllersWithId
  -- DELETE
  --, ControllerEventSelector
) where

import           Relude

import           Reflex
import qualified Reflex.Patch.IntMap             as IM

import           Potato.Flow.Reflex.Manipulators
import           Potato.Flow.SElts

import qualified Data.IntMap.Strict              as IM



type LayerPos = Int
type REltId = Int
type REltIdMap a = IM.IntMap a
type PatchREltIdMap a = IM.PatchIntMap a
type SuperSEltLabel = (REltId, LayerPos, SEltLabel)
type ControllersWithId = IntMap Controller
-- DELETE
--type ControllerEventSelector t = EventSelectorInt t (Controller)
