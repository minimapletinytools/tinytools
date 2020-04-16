{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Data.DirectoryMap (
  DirId
  , DirectoryIdAssigner
  , DirectoryIdAssignerConfig
  , DirectoryMap(..)
  , DirectoryMapConfig(..)
  , holdDirectoryMap
) where

import           Relude

import           Control.Monad.Fix

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.These

import           Reflex



type DirId = Int

-- TODO
-- resposible only for assigning Ids and not for inserting elements
data DirectoryIdAssigner t v  = DirectoryIdAssigner {
  _directoryIdAssigner_assigned :: Event t (NonEmpty (DirId, v))
}

data DirectoryIdAssignerConfig t v  = DirectoryIdAssignerConfig {
  _directoryIdAssignerConfig_assign :: Event t (NonEmpty v)
}

holdDirectoryIdAssigner ::
  forall t m v. (Reflex t, MonadHold t m, MonadFix m)
  => DirectoryIdAssignerConfig t v
  -> m (DirectoryIdAssigner t v)
holdDirectoryIdAssigner DirectoryIdAssignerConfig {..} = mdo
  uid <- foldDyn (\x -> (+ length x)) 0 _directoryIdAssignerConfig_assign
  let
    assigned = attachWith (\firstid -> NE.zip (NE.fromList [firstid..])) (current uid) _directoryIdAssignerConfig_assign
  return
    DirectoryIdAssigner {
        _directoryIdAssigner_assigned = assigned
      }



-- TODO just rename to Directory
data DirectoryMap t v = DirectoryMap {
  _directoryMap_contents  :: Behavior t (Map DirId v)
  , _directoryMap_added   :: Event t (NonEmpty (DirId, v))
  , _directoryMap_removed :: Event t (NonEmpty (DirId, v))
}

data DirectoryMapConfig t v = DirectoryMapConfig {
  -- | add a new element to the directory
  _directoryMapConfig_addNew        :: Event t (NonEmpty v)
  -- | add a element to the directory that was previously removed
  , _directoryMapConfig_addExisting :: Event t (NonEmpty (DirId, v))
  -- | remove an element from the map
  , _directoryMapConfig_remove      :: Event t (NonEmpty DirId)
}

holdDirectoryMap ::
  forall t m v. (Reflex t, MonadHold t m, MonadFix m)
  => DirectoryMapConfig t v
  -> m (DirectoryMap t v)
holdDirectoryMap DirectoryMapConfig {..} = mdo
  uid <- foldDyn (\x -> (+ length x)) 0 _directoryMapConfig_addNew
  let
    newElts = attachWith (\firstid -> NE.zip (NE.fromList [firstid..])) (current uid) _directoryMapConfig_addNew
    -- we merge because it works, but really these two events should never fire at the same time
    add :: Event t (NonEmpty (DirId, v))
    add = mergeWith (<>) [newElts, _directoryMapConfig_addExisting]
    -- lookup each element we are about to remove
    removed = fmap (\(m, els) -> catMaybes . toList . fmap (\i -> (\x -> (i,x)) <$> M.lookup i m) $ els) (attach bDirectory _directoryMapConfig_remove)
    -- setup the directory
    addAndRemove :: Event t (These (NonEmpty (DirId, v)) (NonEmpty DirId))
    addAndRemove = alignEventWithMaybe Just add _directoryMapConfig_remove
    addToMap els m = foldl' (\accm (i,e) -> M.insert i e accm) m els
    removeFromMap els m = foldl' (\accm i -> M.delete i accm) m els
    foldfn :: These (NonEmpty (DirId, v)) (NonEmpty DirId) -> Map DirId v -> Map DirId v
    foldfn (This els) m = addToMap els m
    foldfn (That els) m = removeFromMap els m
    foldfn (These elsadd elsremove) m = addToMap elsadd . removeFromMap elsremove $ m
    bDirectory = current directory
  directory :: Dynamic t (Map DirId v) <-
    foldDyn foldfn M.empty addAndRemove
  return
    DirectoryMap {
        _directoryMap_contents = bDirectory
        , _directoryMap_added = add
        , _directoryMap_removed = fmapMaybe nonEmpty removed
      }
