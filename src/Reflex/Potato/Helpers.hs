{-# LANGUAGE RecursiveDo #-}


module Reflex.Potato.Helpers (
  -- reflex helpers
  leftmostwarn
  , foldDynMergeWith
  , foldDynMerge

  , repeatEvent
) where

import           Relude

import           Reflex

import           Control.Monad.Fix

-- | same as leftmost but outputs a warning if more than one event fires at once
leftmostwarn :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostwarn warning evs = r where
  combine = mergeList evs
  nowarn = fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn = traceEventWith (const warning) $ fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing) combine
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



-- TODO figure out how to actually use this...
-- | triggers output event once for each input event
-- each output event runs in a different consecutive frame
-- if these events trigger the input event, they get appended to the end of events to be triggered
repeatEvent :: (Response m ~ Identity, Request m ~ Identity, Reflex t, Requester t m) => Event t [a] -> m (Event t a)
repeatEvent evin = mdo
  let
    -- if input event fires in subsequent ticks, append to end
    -- obviously, be mindful of infinite loops
    evin' = mergeWith (\rev' ev' -> rev' <> ev') [rev, evin]
    selectNext []    = Nothing
    selectNext (x:_) = Just x
    selectRest []     = Nothing
    selectRest (_:[]) = Nothing
    selectRest (_:xs) = Just xs
    next = fmapMaybe selectNext evin'
    rest = fmapMaybe selectRest evin'
  -- does this mess with keys in DSum when multiple of these trigger?
  rev <- requestingIdentity (Identity <$> rest)
  requestingIdentity (Identity <$> next)
