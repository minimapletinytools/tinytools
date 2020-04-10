module Reflex.TestHarness (
  TestApp
  , playReflex
) where

import           Relude


import           Reflex
import           Reflex.Host.Class
import           Reflex.Spider

import           Control.Monad.Fix
import           Data.Dependent.Sum
import qualified Data.Traversable   as T



type TestApp t m a =
  (Reflex t, MonadHold t m, MonadFix m)
  => Event t () -- ^ input event, triggered once at start
  -> m (Event t a) -- ^ final event we want to listen to

playReflex ::
  forall a. (Show a)
  => (forall t m. TestApp t m a) -- ^ entry point into reflex app
  -> IO (Maybe a)
playReflex network =
  runSpiderHost $ do
    (tickEvent,  tickTriggerRef)  <- newEventWithTriggerRef
    tickEventHandle <- subscribeEvent tickEvent

    finalEvent <- runHostFrame $ network tickEvent
    finalEventHandle <- subscribeEvent finalEvent

    -- you could just use this instead
    --fireEventRef
    do
      trig <- readIORef tickTriggerRef
      let
        eventValue :: (MonadReadEvent t m, Show a) => EventHandle t a -> m (Maybe a)
        eventValue = readEvent >=> T.sequenceA
      final <- case trig of
        Nothing -> error "no trigger ref"
        Just t  -> fireEventsAndRead [t :=> Identity ()] $ eventValue finalEventHandle
      case final of
        Nothing -> return Nothing
        Just x  -> return (Just x)
