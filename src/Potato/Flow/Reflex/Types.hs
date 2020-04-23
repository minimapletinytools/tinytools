module Potato.Flow.Reflex.Types (
  LayerPos
  , REltId
  , REltIdMap
  , PatchREltIdMap
  , SuperSEltLabel
  , ControllersWithId
) where

import           Relude

import           Potato.Flow.Reflex.Manipulators
import           Potato.Flow.SElts

import qualified Data.IntMap.Strict              as IM

import qualified Reflex.Patch.IntMap             as IM

type LayerPos = Int
type REltId = Int
type REltIdMap a = IM.IntMap a
type PatchREltIdMap a = IM.PatchIntMap a
type SuperSEltLabel = (REltId, LayerPos, SEltLabel)
type ControllersWithId = IntMap Controllers
