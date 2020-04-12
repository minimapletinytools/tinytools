-- various potato helpers
module Reflex.Potato (
  leftmostwarn
) where

import           Relude

import           Reflex


-- | same as leftmost but outputs a warning if more than one event fires at once
leftmostwarn :: (Reflex t) => String -> [Event t a] -> Event t a
leftmostwarn warning evs = r where
  combine = mergeList evs
  nowarn = fmapMaybe (\x -> if length x == 1 then Just (head x) else Nothing) combine
  warn = fmapMaybe (\x -> if length x > 1 then Just (head x) else Nothing) combine
  r = traceEventWith (const warning) (leftmost [nowarn, warn])
