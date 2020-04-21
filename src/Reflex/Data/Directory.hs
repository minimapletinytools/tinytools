{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Data.Directory (
  DirId
  , DirectoryIdAssigner(..)
  , DirectoryIdAssignerConfig(..)
  , holdDirectoryIdAssigner
  , Directory(..)
  , DirectoryConfig(..)
  , holdDirectory
) where

import           Relude

import           Control.Monad.Fix

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.These

import           Reflex


-- TODO change this to enum type class
type DirId = Int

data DirectoryIdAssigner t v  = DirectoryIdAssigner {
  -- DELETE
  --_directoryIdAssigner_assigned :: Dynamic t (NonEmpty (DirId, v))

  -- | tag an event with ided elements that got generate by the event
  -- the input event must be assigned to this directory
  -- i.e. it will be an event that is passed into
  -- '_directoryIdAssignerConfig_assign' in some way
  _directoryIdAssigner_tag :: forall a. Event t a -> Event t (NonEmpty (DirId, v))
}

data DirectoryIdAssignerConfig t v  = DirectoryIdAssignerConfig {
  _directoryIdAssignerConfig_assign :: Event t (NonEmpty v)
}

holdDirectoryIdAssigner ::
  forall t m v. (Reflex t, MonadHold t m, MonadFix m)
  => DirectoryIdAssignerConfig t v
  -> m (DirectoryIdAssigner t v)
holdDirectoryIdAssigner DirectoryIdAssignerConfig {..} = do
  uid <- foldDyn (\x -> (+ length x)) 0 _directoryIdAssignerConfig_assign
  --let assigned = attachWith (\firstid -> NE.zip (NE.fromList [firstid..])) (current uid) _directoryIdAssignerConfig_assign
  --dAssigned <- holdDyn ((-1,undefined) :| []) assigned
  let
    attached :: Event t (NonEmpty (DirId, v))
    attached = attachWith (\firstid -> NE.zip (NE.fromList [firstid..])) (current uid) _directoryIdAssignerConfig_assign
    alignfn = \case
      These a _ -> Just a
      _ -> Nothing
    maketag :: forall a. Event t a -> Event t (NonEmpty (DirId, v))
    maketag = alignEventWithMaybe alignfn attached
  return
    DirectoryIdAssigner {
        --_directoryIdAssigner_assigned = dAssigned
        _directoryIdAssigner_tag = maketag
      }


data Directory t v = Directory {
  _directoryMap_contents  :: Behavior t (Map DirId v)
  , _directoryMap_added   :: Event t (NonEmpty (DirId, v))
  , _directoryMap_removed :: Event t (NonEmpty (DirId, v))
}

data DirectoryConfig t v = DirectoryConfig {
  -- | add a element to the directory
  -- ensure the DirId was assigned from the same instance of DirectoryIdAssigner
  _directoryMapConfig_add      :: Event t (NonEmpty (DirId, v))
  -- | remove an element from the map
  , _directoryMapConfig_remove :: Event t (NonEmpty DirId)
}

holdDirectory ::
  forall t m v. (Reflex t, MonadHold t m, MonadFix m)
  => DirectoryConfig t v
  -> m (Directory t v)
holdDirectory DirectoryConfig {..} = mdo
  let
    add :: Event t (NonEmpty (DirId, v))
    add = _directoryMapConfig_add
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
    Directory {
        _directoryMap_contents = bDirectory
        , _directoryMap_added = add
        , _directoryMap_removed = fmapMaybe nonEmpty removed
      }
