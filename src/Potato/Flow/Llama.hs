-- WIP


module Potato.Flow.Llama where

import           Relude

import           Potato.Flow.Owl
import           Potato.Flow.SElts
import Potato.Flow.OwlState
import           Potato.Flow.Types

import qualified Data.IntMap as IM

data SLlama = SLlama_Set [(REltId, SElt)] | SLlama_Rename (REltId, Text)

data ApplyLlamaError = ApplyLlamaError_Generic Text

data Llama = Llama {
  _llama_apply :: OwlPFState -> Either ApplyLlamaError (OwlPFState, SuperOwlChanges, Llama)
  , _llama_serialize :: SLlama
}

makeRenameLlama :: (REltId, Text) -> Llama
makeRenameLlama (rid, newname) = r where

  apply pfs = let
      mapping = _owlTree_mapping . _owlPFState_owlTree $ pfs
    in case IM.lookup rid mapping of
        Nothing -> Left $ ApplyLlamaError_Generic $ "Element to rename does not exist " <> show rid
        Just (oldoem, oldoe) -> let
            (newoe, oldname) = case oldoe of
              OwlEltFolder oi kiddos -> (OwlEltFolder (oi { _owlInfo_name = newname}) kiddos, _owlInfo_name oi)
              OwlEltSElt oi selt -> (OwlEltSElt (oi { _owlInfo_name = newname}) selt, _owlInfo_name oi)
            newsowl = SuperOwl rid oldoem newoe
            newMapping = IM.insert rid (oldoem, newoe) mapping
            changes = IM.singleton rid (Just newsowl)
            unset = makeRenameLlama (rid, oldname)
            newState = pfs { _owlPFState_owlTree = (_owlPFState_owlTree pfs) { _owlTree_mapping = newMapping } }
          in
            Right $ (newState, changes, unset)


  serialize = SLlama_Rename (rid, newname)
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
    }


makeSetLlama :: (REltId, SElt) -> Llama
makeSetLlama (rid, selt) = r where
  apply pfs = let
      mapping = _owlTree_mapping . _owlPFState_owlTree $ pfs
    in case IM.lookup rid mapping of
        Nothing -> Left $ ApplyLlamaError_Generic $ "Element to modify does not exist " <> show rid
        Just (_, OwlEltFolder _ _) -> Left $ ApplyLlamaError_Generic $ "Element to modify is a folder " <> show rid
        Just (oldoem, OwlEltSElt oi oldselt) -> let
            newoe = OwlEltSElt oi selt
            newsowl = SuperOwl rid oldoem newoe
            newMapping = IM.insert rid (oldoem, newoe) mapping
            changes = IM.singleton rid (Just newsowl)
            unset = makeSetLlama (rid, oldselt)
            newState = pfs { _owlPFState_owlTree = (_owlPFState_owlTree pfs) { _owlTree_mapping = newMapping } }
          in
            Right $ (newState, changes, unset)


  serialize = SLlama_Set [(rid, selt)]
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
    }



sLlama_deserialize :: SLlama -> Llama
sLlama_deserialize (SLlama_Set xs) = undefined
