{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Data.Sequence (
  DynamicSeq(..)
  , DynamicSeqConfig(..)
  , defaultDynamicSeqConfig
  , holdDynamicSeq
) where

import           Relude                hiding (empty, splitAt)

import           Reflex
import           Reflex.Potato.Helpers

import           Control.Monad.Fix

import           Data.Sequence
import           Data.Wedge


-- maybe make a simple variant of this that only supports adding one at a time

-- | Dynamic variant of Seq
-- the interface is modified to support adding/removing several elements at once
-- this is to avoid needing to use 'runWithReplace' to do this in one event tick
data DynamicSeq t a = DynamicSeq {
  -- | index and sub sequence that was just added
  _dynamicSeq_inserted   :: Event t (Int, Seq a)
  -- | original index of removed sub sequence and removed subsequence
  , _dynamicSeq_removed  :: Event t (Int, Seq a)
  -- TODO
  -- though you can probably do this by adding + removing with runWithReplace
  --, _dynamicSeq_moved     :: Event t (Int, a)
  -- | internal state of contents
  , _dynamicSeq_contents :: Dynamic t (Seq a)
}

data DynamicSeqConfig t a = DynamicSeqConfig {
  -- | index and sub sequence to add
  _dynamicSeqConfig_insert   :: Event t (Int, Seq a)
  -- | index and number of elements to remove
  , _dynamicSeqConfig_remove :: Event t (Int, Int)
  -- | same as removing all elts
  , _dynamicSeqConfig_clear  :: Event t ()

  -- TODO
  --, _dynamicSeqConfig_move    :: Event t (Int,Int)
}

defaultDynamicSeqConfig :: (Reflex t) => DynamicSeqConfig t a
defaultDynamicSeqConfig = DynamicSeqConfig {
    _dynamicSeqConfig_insert = never
    , _dynamicSeqConfig_remove = never
    , _dynamicSeqConfig_clear = never
  }


type DSState a = (Wedge (Int, Seq a) (Int, Seq a), Seq a)
data DSCmd t a = DSCAdd (Int, Seq a) | DSCRemove (Int, Int) | DSCClear

-- | create a dynamic list
holdDynamicSeq ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => Seq a
  -> DynamicSeqConfig t a
  -> m (DynamicSeq t a)
holdDynamicSeq initial DynamicSeqConfig {..} = mdo
  let
    changeEvent :: Event t (DSCmd t a)
    changeEvent = leftmostwarn "WARNING: multiple Seq events firing at once" [
        fmap DSCAdd _dynamicSeqConfig_insert
        , fmap DSCRemove _dynamicSeqConfig_remove
        , fmap (const DSCClear) _dynamicSeqConfig_clear
      ]

    -- Wedge values:
    -- Here is elements that was just added fromSeq
    -- There is elements that was just removed from Seq
    -- Nowhere is everything else
    foldfn :: (DSCmd t a) -> DSState a -> PushM t (DSState a)
    foldfn (DSCAdd (i, ys)) (_, xs)  = return (Here (i, ys), newSeq) where
      (l, r) = splitAt i xs
      newSeq = l >< xs >< r
    foldfn (DSCRemove (i, n)) (_, xs) = return (There (i, removed), newSeq) where
      (keepl, rs) = splitAt i xs
      (removed, keepr) = splitAt n rs
      newSeq = keepl >< keepr
    foldfn DSCClear (_, xs) = return (There (0, xs), empty)

  asdyn :: Dynamic t (DSState a) <-
    foldDynM foldfn (Nowhere, initial) changeEvent

  return $ DynamicSeq {
      _dynamicSeq_inserted = never
      , _dynamicSeq_removed = never
      , _dynamicSeq_contents = snd <$> asdyn
    }
