{-# LANGUAGE RecordWildCards #-}


module Potato.Flow.Llama where

import           Relude                   hiding (state)

import           Potato.Flow.DebugHelpers
import           Potato.Flow.Math
import           Potato.Flow.Owl
import           Potato.Flow.OwlItem
import           Potato.Flow.OwlState
import           Potato.Flow.Serialization.Snake
import           Potato.Flow.Types


import           Control.Exception        (assert)
import qualified Data.IntMap              as IM
import qualified Data.Text                as T
import qualified Text.Show


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
  --  | OwlPFCSnap (OwlPFState, OwlPFState) --(before, after)
  deriving (Show, Generic)

instance NFData OwlPFCmd




-- | returns true if the applying DeltaLBox results in a valid canvas size
validateCanvasSizeOperation :: DeltaLBox -> OwlPFState -> Bool
validateCanvasSizeOperation lbox pfs = r where
  oldcanvas = _sCanvas_box $ _owlPFState_canvas pfs
  newcanvas = plusDelta oldcanvas lbox
  r = isValidCanvas (SCanvas newcanvas)

doCmdState :: OwlPFCmd -> OwlPFState -> Either ApplyLlamaError (OwlPFState, SuperOwlChanges)
doCmdState cmd s = r where
  r' = case cmd of

    OwlPFCNewElts x      ->  Right $ do_newElts x s
    OwlPFCDeleteElts x   ->  Right $ do_deleteElts x s

    OwlPFCNewTree x      -> Right $ do_newMiniOwlTree x s
    OwlPFCDeleteTree x   -> Right $ do_deleteMiniOwlTree x s

    OwlPFCManipulate x   ->  Right $ do_manipulate x s

    OwlPFCMove x         -> Right $ do_move x s
    OwlPFCResizeCanvas x -> if validateCanvasSizeOperation x s 
      then Right $ (do_resizeCanvas x s, IM.empty)
      else Left $ ApplyLLamaError_Soft $ "Invalid canvas size operation " <> show x

  r = case r' of 
    Right (newState, _) -> assert (owlPFState_isValid newState) r'
    Left e -> Left e

undoCmdState :: OwlPFCmd -> OwlPFState -> Either ApplyLlamaError (OwlPFState, SuperOwlChanges)
undoCmdState cmd s = r where
  r' =  case cmd of

    OwlPFCNewElts x      ->  Right $ undo_newElts x s
    OwlPFCDeleteElts x   ->  Right $ undo_deleteElts x s

    OwlPFCNewTree x      -> Right $ undo_newMiniOwlTree x s
    OwlPFCDeleteTree x   -> Right $ undo_deleteMiniOwlTree x s

    OwlPFCManipulate x   -> Right $ undo_manipulate x s

    OwlPFCMove x         -> Right $ undo_move x s
    OwlPFCResizeCanvas x -> if validateCanvasSizeOperation (deltaLBox_invert x) s 
      then Right $ (undo_resizeCanvas x s, IM.empty)
      else Left $ ApplyLLamaError_Soft $ "Invalid canvas size operation " <> show x

  r = case r' of
    Right (newState, _) -> assert (owlPFState_isValid newState) r'
    Left e                    -> Left e




data SLlama =
  SLlama_Set [(REltId, SElt)]
  | SLlama_Rename (REltId, Text)
  | SLlama_Compose [SLlama]
  | SLlama_OwlPFCmd OwlPFCmd Bool
  deriving (Show, Generic)

instance NFData SLlama

data ApplyLlamaError = ApplyLlamaError_Fatal Text | ApplyLLamaError_Soft Text deriving (Show)

data Llama = Llama {
  _llama_apply :: OwlPFState -> Either ApplyLlamaError (OwlPFState, SuperOwlChanges, Llama)
  , _llama_serialize :: SLlama
  , _llama_describe :: Text
} deriving (Generic)

instance NFData Llama

instance Show Llama where
  show = show . _llama_serialize

data LlamaStack = LlamaStack {
  _llamaStack_done        :: [Llama] -- stuff we've done, applying these Llamas will *undo* the operation that put them on the stack!
  , _llamaStack_undone    :: [Llama] -- stuff we've undone, applying these Llamas will *redo* the operation that put them on the stack!
  , _llamaStack_lastSaved :: Maybe Int -- size of do stacks on last save
} deriving (Show, Generic)

instance NFData LlamaStack

emptyLlamaStack :: LlamaStack
emptyLlamaStack = LlamaStack [] [] (Just 0)


-- UNTESTED
llamaStack_hasUnsavedChanges :: LlamaStack -> Bool
llamaStack_hasUnsavedChanges LlamaStack {..} = case _llamaStack_lastSaved of
  Nothing -> True
  Just x  -> x /= length _llamaStack_done

makeRenameLlama :: (REltId, Text) -> Llama
makeRenameLlama (rid, newname) = r where

  apply pfs = let
      mapping = _owlTree_mapping . _owlPFState_owlTree $ pfs
    in case IM.lookup rid mapping of
        Nothing -> Left $ ApplyLlamaError_Fatal $ "Element to rename does not exist " <> show rid
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
      , _llama_describe = "rename " <> show rid <> " to " <> newname
    }

makeSetLlama :: (REltId, SElt) -> Llama
makeSetLlama (rid, selt) = r where
  apply pfs = let
      mapping = _owlTree_mapping . _owlPFState_owlTree $ pfs
    in case IM.lookup rid mapping of
        Nothing -> Left $ ApplyLlamaError_Fatal $ "Element to modify does not exist " <> show rid <> " " <> potatoShow (_owlPFState_owlTree $ pfs)
        Just (_, OwlItem _ (OwlSubItemFolder _)) -> Left $ ApplyLlamaError_Fatal $ "Element to modify is a folder " <> show rid
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
      , _llama_describe = "set " <> show rid <> " to " <> show selt
    }


makePFCLlama' :: Bool -> OwlPFCmd -> Llama
makePFCLlama' isDo cmd = r where
  apply pfs = let
      unset = makePFCLlama' (not isDo) cmd
    in case (if isDo then doCmdState cmd pfs else undoCmdState cmd pfs) of
      Right (newState, changes) -> Right $ (newState, changes, unset)
      Left e -> Left e


        
  serialize = SLlama_OwlPFCmd cmd isDo
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
      , _llama_describe = "PFC " <> show cmd
    }

makePFCLlama :: OwlPFCmd -> Llama
makePFCLlama = makePFCLlama' True

-- UNTESTED
makeCompositionLlama :: [Llama] -> Llama
makeCompositionLlama llamas = r where

  apply pfs = go llamas (pfs, IM.empty, []) where
    go [] (state, changes, undollamas) = Right (state, changes, makeCompositionLlama undollamas)
    go (llama:rest) (state, changes, undollamas) = case _llama_apply llama state of
      Right (newstate, newchanges, newundollama) -> go rest (newstate, IM.union newchanges changes, newundollama:undollamas)
      e -> e


  serialize = SLlama_Compose $ fmap _llama_serialize llamas
  r = Llama {
      _llama_apply = apply
      , _llama_serialize = serialize
      , _llama_describe = T.concat $ fmap _llama_describe llamas
    }


sLlama_deserialize :: OwlPFState -> SLlama -> Llama
sLlama_deserialize pfs sllama = case sllama of
  SLlama_Set pairs -> makeCompositionLlama (fmap makeSetLlama pairs)
  SLlama_Rename x -> makeRenameLlama x
  SLlama_Compose x -> makeCompositionLlama $ fmap (sLlama_deserialize pfs) x
  SLlama_OwlPFCmd pfc isDo -> makePFCLlama' isDo pfc
