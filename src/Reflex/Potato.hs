-- various potato helpers
module Reflex.Potato (
  leftmostwarn
  , foldDynMergeWith
  , foldDynMerge
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
