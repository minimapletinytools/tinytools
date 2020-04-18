{-# LANGUAGE RecursiveDo #-}


module Reflex.Potato.Helpers (
  -- reflex helpers
  leftmostwarn
  , foldDynMergeWith
  , foldDynMerge

  , fanDSum

  , repeatEvent
  , repeatEventAndCollectOutput
) where

import           Relude

import           Reflex

import           Control.Monad.Fix

import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS


-- | same as leftmost but outputs a warning if more than one event fires at once
leftmostwarn :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostwarn label evs = r where
  combine = mergeList evs
  nowarn = fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn = traceEventWith (const ("WARNING: multiple " <> label <> " events triggered")) $ fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing) combine
  r = leftmost [nowarn, warn]

foldDynMergeWith :: (Reflex t, MonadHold t m, MonadFix m)
  => b -- ^ initial value of dynamic
  -> [Event t (b -> b)]  -- ^ list of events producing a reducing method
  -> m (Dynamic t b)  -- ^ final output after all folding methods applied
foldDynMergeWith acc = foldDyn ($) acc . mergeWith (.)

foldDynMerge :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> b) -- ^ folding method
  -> b -- ^ initial value of dynamic
  -> [Event t a] -- ^ list of events
  -> m (Dynamic t b) -- ^ final output
foldDynMerge f acc evs = foldDynMergeWith acc (f <<$>> evs)

fanDSum :: forall t k. (Reflex t, DM.GCompare k)
  => Event t (DS.DSum k Identity)
  -> EventSelector t k
fanDSum ds = fan $ DM.fromAscList . (:[]) <$> ds


selectNext :: [a] -> Maybe a
selectNext []    = Nothing
selectNext (x:_) = Just x
selectRest :: [a] -> Maybe [a]
selectRest []     = Nothing
selectRest (_:[]) = Nothing
selectRest (_:xs) = Just xs

-- TODO rename
-- TODO prob implement with repeatEventAndCollectOutput
-- lazy evaluation should mean it's no less efficient but IDK
-- | triggers output event once for each input event
-- each output event runs in a different consecutive frame
-- if these events trigger the input event, they get appended to the end of events to be triggered
repeatEvent :: forall t m a. (Reflex t, Adjustable t m, MonadFix m) => Event t [a] -> m (Event t a)
repeatEvent evin = mdo
  let
    -- if input event fires in subsequent ticks, append to end
    -- obviously, be mindful of infinite loops
    evin' :: Event t [a]
    evin' = mergeWith (\rev' ev' -> rev' <> ev') [rev, evin]
    next = fmapMaybe selectNext evin'
    rest = fmapMaybe selectRest evin'

  -- TODO this implementation is better but I can't figure out how to properly wrap request and response types
  --rev <- requestingIdentity (Identity <$> rest)
  --requestingIdentity (Identity <$> next)

  (_, rev) <- runWithReplace (return ()) (return <$> rest)
  return next

-- collected result event triggers simultaneously with last event in list to repeat
repeatEventAndCollectOutput ::
  forall t m a b. (Adjustable t m, MonadHold t m, MonadFix m)
  => Event t [a] -- ^ event to repeat
  -> Event t b -- ^ event to collect results from, only collects if event fires
  -> m (Event t a, Event t [b]) -- ^ (repeated event, collected results once event is done repeating)
repeatEventAndCollectOutput evin collectEv = mdo
  let
    -- if input event fires in subsequent ticks, append to end
    -- obviously, be mindful of infinite loops
    evin' :: Event t [a]
    evin' = mergeWith (\rev' ev' -> rev' <> ev') [rev, evin]
    next = fmapMaybe selectNext evin'
    rest = fmapMaybe selectRest evin'
    -- nothing left, this means we fired the last event
    stop = fmapMaybe (\x -> if null x then Just () else Nothing) evin'
    collected = tagPromptlyDyn (reverse <$> collector) stop
  -- collect events in reverse order
  collector <- foldDyn (:) [] collectEv
  (_, rev) <- runWithReplace (return ()) (return <$> rest)
  return (next, collected)
