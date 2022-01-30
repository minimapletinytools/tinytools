{-# LANGUAGE RecordWildCards #-}

-- WIP


module Potato.Flow.Llama where

import           Relude

import           Potato.Flow.Owl
import           Potato.Flow.SElts
import Potato.Flow.OwlState
import           Potato.Flow.Types
import Potato.Flow.Math

import qualified Data.IntMap as IM
import qualified Text.Show
import           Control.Exception  (assert)


-- TODO rename
-- TODO this is a carryover from a refactor, it would be good to combine this with SLlama but I won't bother.
data OwlPFCmd =
  OwlPFCNewElts [(REltId, OwlSpot, OwlElt)]
  | OwlPFCDeleteElts [(REltId, OwlSpot, OwlElt)]

  | OwlPFCNewTree (MiniOwlTree, OwlSpot)
  | OwlPFCDeleteTree (MiniOwlTree, OwlSpot)

  -- DEPRECATE
  | OwlPFCManipulate ControllersWithId

  -- we need SuperOwlParliament for undo
  | OwlPFCMove (OwlSpot, SuperOwlParliament)

  | OwlPFCResizeCanvas DeltaLBox

  -- | OwlPFCSnap (OwlPFState, OwlPFState) --(before, after)
  deriving (Show, Generic)

instance NFData OwlPFCmd

doCmdState :: OwlPFCmd -> OwlPFState -> (OwlPFState, SuperOwlChanges)
doCmdState cmd s = assert (owlPFState_isValid newState) (newState, changes) where
  (newState, changes) = case cmd of

    OwlPFCNewElts x      ->  do_newElts x s
    OwlPFCDeleteElts x   ->  do_deleteElts x s

    OwlPFCNewTree x -> do_newMiniOwlTree x s
    OwlPFCDeleteTree x -> do_deleteMiniOwlTree x s

    OwlPFCManipulate x   ->  do_manipulate x s

    OwlPFCMove x         -> do_move x s
    OwlPFCResizeCanvas x -> (do_resizeCanvas x s, IM.empty)

undoCmdState :: OwlPFCmd -> OwlPFState -> (OwlPFState, SuperOwlChanges)
undoCmdState cmd s = assert (owlPFState_isValid newState) (newState, changes) where
  (newState, changes) =  case cmd of

    OwlPFCNewElts x      ->  undo_newElts x s
    OwlPFCDeleteElts x   ->  undo_deleteElts x s

    OwlPFCNewTree x -> undo_newMiniOwlTree x s
    OwlPFCDeleteTree x -> undo_deleteMiniOwlTree x s

    OwlPFCManipulate x   ->  undo_manipulate x s

    OwlPFCMove x         -> undo_move x s
    OwlPFCResizeCanvas x -> (undo_resizeCanvas x s, IM.empty)



data SLlama = SLlama_Set [(REltId, SElt)] | SLlama_Rename (REltId, Text) | SLlama_Compose [SLlama] | SLlama_OwlPFCmd OwlPFCmd deriving (Show, Generic)

instance NFData SLlama

data ApplyLlamaError = ApplyLlamaError_Generic Text

data Llama = Llama {
  _llama_apply :: OwlPFState -> Either ApplyLlamaError (OwlPFState, SuperOwlChanges, Llama)
  , _llama_serialize :: SLlama
} deriving (Generic)

instance NFData Llama

instance Show Llama where
  show = show . _llama_serialize

data LlamaStack = LlamaStack {
  _llamaStack_do :: [Llama] -- stuff we've done, applying these Llamas will *undo* the operation that put them on the stack!
  , _llamaStack_undo :: [Llama] -- stuff we've undone, applying these Llamas will *redo* the operation that put them on the stack!
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


makePFCLlama' :: Bool -> OwlPFCmd -> Llama
makePFCLlama' isDo cmd = r where
  apply pfs = let
      unset = makePFCLlama' (not isDo) cmd
      (newState, changes) = if isDo then doCmdState cmd pfs else undoCmdState cmd pfs
    in Right $ (newState, changes, unset)

  serialize = SLlama_OwlPFCmd cmd
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
    }

makePFCLlama :: OwlPFCmd -> Llama
makePFCLlama = makePFCLlama' True

-- TODO finish
makeCompositionLlama :: [Llama] -> Llama
makeCompositionLlama llamas = r where

  -- TODO mapaccum whatever
  apply pfs = undefined

  serialize = SLlama_Compose $ fmap _llama_serialize llamas
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
    }


sLlama_deserialize :: OwlPFState -> SLlama -> Llama
sLlama_deserialize _ sllama = case sllama of
  SLlama_Set pairs -> makeCompositionLlama (fmap makeSetLlama pairs)
  SLlama_Rename x -> makeRenameLlama x
