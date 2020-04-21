{-# LANGUAGE RecursiveDo #-}


module Reflex.Potato.Helpers (
  -- other helpers
  dsum_to_dmap


  -- reflex helpers
  , traceEventSimple
  , leftmostwarn
  , alignEitherWarn
  , foldDynMergeWith
  , foldDynMerge

  , fanDSum

  , sequenceEvents
  , repeatEvent
  , repeatEventAndCollectOutput
) where

import           Relude

import           Reflex

import           Control.Monad.Fix

import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS
import           Data.These


dsum_to_dmap :: DM.GCompare k => DS.DSum k f -> DM.DMap k f
dsum_to_dmap ds = DM.fromList [ds]

traceEventSimple :: (Reflex t) => String -> Event t a -> Event t a
traceEventSimple s = traceEventWith (const s)

-- TODO rename leftmostWarn
-- | same as leftmost but outputs a warning if more than one event fires at once
leftmostwarn :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostwarn label evs = r where
  combine = mergeList evs
  nowarn = fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn = traceEventWith (const ("WARNING: multiple " <> label <> " events triggered")) $ fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing) combine
  r = leftmost [nowarn, warn]

-- | same as align but only returns left event if both events fire
-- prints a warning if both events fire
alignEitherWarn :: (Reflex t) => String -> Event t a -> Event t b -> Event t (Either a b)
alignEitherWarn label ev1 ev2 = leftmostwarn label [Left <$> ev1, Right <$> ev2]

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

-- TODO this seems to cause leaks, test it
-- | if both events fire at the same time, this functions returns an event with
-- the second event's results that fires one frame after the first event fires
sequenceEvents :: forall t m a b. (Reflex t, Adjustable t m, MonadFix m) => Event t a -> Event t b -> m (Event t b)
sequenceEvents ev1 ev2 = mdo
  let
    makeEv2Delayed :: m (Event t b)
    makeEv2Delayed = do
      let
        -- filters for when BOTH ev1 and ev2 triggered in the previous frame
        fmapfn = \case
          These v1 v2 -> Just v2
          _ -> Nothing
        delayed = fmapMaybe fmapfn redo
      -- if ev1 does not trigger, delay does not trigger and this gives ev2
      -- if ev1 did trigger, and ev2 did not, this gives ev2
      -- if ev1 and ev2 both triggered, this gives previous value of evl2
      -- * note that it's possible for ev1 or ev2 to trigger in the second frame for outside reasons
      -- if this is the case, you really should not use this function
      return $ leftmost [delayed, difference ev2 ev1]
  (ev2Delayed, redo) <- runWithReplace makeEv2Delayed (alignEventWithMaybe (Just . return) ev1 ev2)
  return ev2Delayed

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
    stop = fmapMaybe (\x -> if isNothing (selectRest x) then Just () else Nothing) evin'
    collected = tagPromptlyDyn (reverse <$> collector) stop

    -- collect events in reverse order
    -- reset when given the signal
    foldfn :: These Bool b -> [b] -> [b]
    foldfn (This True) _      = []
    foldfn (That b) bs        = b : bs
    foldfn (These True b) _   = [b]
    foldfn (These False b) bs = b : bs
    foldfn _ bs               = bs

  -- we use the trick 'tag (current resetState) evin''
  -- which causes it to use resetState from previous iterations.
  collector <- foldDyn foldfn [] (alignEventWithMaybe Just (tag (current resetState) evin') collectEv)

  resetState <- foldDyn const True (leftmost [const True <$> stop, const False <$> evin'])

  (_, rev) <- runWithReplace (return ()) (return <$> rest)
  return (next, collected)
