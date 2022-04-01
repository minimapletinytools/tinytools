{-# LANGUAGE RecordWildCards #-}

-- WIP


module Potato.Flow.Llama where

import           Relude

import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import Potato.Flow.OwlItem
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
  OwlPFCNewElts [(REltId, OwlSpot, OwlItem)]
  | OwlPFCDeleteElts [(REltId, OwlSpot, OwlItem)]

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



data SLlama =
  SLlama_Set [(REltId, SElt)]
  | SLlama_Rename (REltId, Text)
  | SLlama_Compose [SLlama]
  -- TODO OwlItem contains caches which we don't want to so serialize with Llama so ideally there should be a mirrored type to remove the cache, should be able to use SElt equivalents here instead?
  | SLlama_OwlPFCmd OwlPFCmd Bool
  deriving (Show, Generic)

instance NFData SLlama

data ApplyLlamaError = ApplyLlamaError_Generic Text deriving (Show)

data Llama = Llama {
  _llama_apply :: OwlPFState -> Either ApplyLlamaError (OwlPFState, SuperOwlChanges, Llama)
  , _llama_serialize :: SLlama
} deriving (Generic)

instance NFData Llama

instance Show Llama where
  show = show . _llama_serialize

data LlamaStack = LlamaStack {
  _llamaStack_done :: [Llama] -- stuff we've done, applying these Llamas will *undo* the operation that put them on the stack!
  , _llamaStack_undone :: [Llama] -- stuff we've undone, applying these Llamas will *redo* the operation that put them on the stack!
  , _llamaStack_lastSaved :: Maybe Int -- size of do stacks on last save
} deriving (Show, Generic)

instance NFData LlamaStack

emptyLlamaStack :: LlamaStack
emptyLlamaStack = LlamaStack [] [] (Just 0)


-- UNTESTED
llamaStack_hasUnsavedChanges :: LlamaStack -> Bool
llamaStack_hasUnsavedChanges LlamaStack {..} = case _llamaStack_lastSaved of
  Nothing -> True
  Just x -> x /= length _llamaStack_done

makeRenameLlama :: (REltId, Text) -> Llama
makeRenameLlama (rid, newname) = r where

  apply pfs = let
      mapping = _owlTree_mapping . _owlPFState_owlTree $ pfs
    in case IM.lookup rid mapping of
        Nothing -> Left $ ApplyLlamaError_Generic $ "Element to rename does not exist " <> show rid
        Just (oldoem, oldoitem) -> let
            (newoitem, oldname) = (owlItem_setName oldoitem newname, owlItem_name oldoitem)
            newsowl = SuperOwl rid oldoem newoitem
            newMapping = IM.insert rid (oldoem, newoitem) mapping
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
        Just (_, OwlItem _ (OwlSubItemFolder _)) -> Left $ ApplyLlamaError_Generic $ "Element to modify is a folder " <> show rid
        Just (oldoem, OwlItem oinfo oldsubitem) -> let
            -- this will clear the cache in OwlItem
            newoitem = OwlItem oinfo $ sElt_to_owlSubItem selt
            newsowl = SuperOwl rid oldoem newoitem
            newMapping = IM.insert rid (oldoem, newoitem) mapping
            changes = IM.singleton rid (Just newsowl)
            unset = makeSetLlama (rid, owlSubItem_to_sElt_hack oldsubitem)
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

  serialize = SLlama_OwlPFCmd cmd isDo
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
sLlama_deserialize pfs sllama = case sllama of
  SLlama_Set pairs -> makeCompositionLlama (fmap makeSetLlama pairs)
  SLlama_Rename x -> makeRenameLlama x
  SLlama_Compose x -> makeCompositionLlama $ fmap (sLlama_deserialize pfs) x
  SLlama_OwlPFCmd pfc isDo -> makePFCLlama' isDo pfc
