module Potato.Flow.DebugHelpers where

import           Relude
import Control.Monad.Writer

-- prob just replace with show? Why do you ever want to not potato show?
-- the reason is becaues potatoshow doesn't show all information, but whatever it's fine
class PotatoShow a where
  potatoShow :: a -> Text

showFoldable :: (Foldable f, Show a) => f a -> Text
showFoldable = foldl' (\acc x -> acc <> show x <> "\n") ""

assertShowAndDump  :: (HasCallStack, Show a) => a -> Bool -> b -> b
assertShowAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> show a

assertPotatoShowAndDump :: (HasCallStack, PotatoShow a) => a -> Bool -> b -> b
assertPotatoShowAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> potatoShow a

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x


-- TODO decide if we want to use this??? Too much???
data PotatoLoggerLevel = PLL_Debug | PLL_Info | PLL_Warn | PLL_Error
data PotatoLoggerComponent = PLC_None | PLC_Goat

class MonadPotatoLogger m where
  potatoLog :: PotatoLoggerLevel -> PotatoLoggerComponent -> Text -> m ()

data PotatoLoggerObject = PotatoLoggerObject PotatoLoggerLevel PotatoLoggerComponent Text

type PotatoLogger = Writer [PotatoLoggerObject]

instance MonadPotatoLogger PotatoLogger where
  potatoLog l c t = tell $ [PotatoLoggerObject l c t]
