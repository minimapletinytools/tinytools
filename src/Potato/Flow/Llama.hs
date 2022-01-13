-- WIP


module Potato.Flow.Llama

import           Relude

import           Potato.Flow.Types

data SLlama = SLlama_Set [(REltId, SEltLabel)]

data Llama = Llama {
  _llama_apply :: OwlPFState -> (OwlPFState, SuperOwlChanges, Llama)
  , _llama_serialize :: SLlama
}

makeSetLlama :: (REltId, SEltLabel) -> Llama
makeSetLlama (rid, seltl) = r where
  apply = undefined
  serialize = SLama_Set [(rid, seltl)]
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
    }



sLlama_deserialize :: SLlama -> Llama
sLlama_deserialize (SLama_Set xs) = undefined
