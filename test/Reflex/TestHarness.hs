module Reflex.TestHarness (
  TestApp
  , playReflex
  , playReflexSeq
) where

import           Relude


import           Reflex
import           Reflex.Host.Class
import           Reflex.Spider

import           Control.Monad.Fix
import           Data.Dependent.Sum
import qualified Data.Traversable   as T


type TestApp t m b a =
  (Reflex t, MonadHold t m, MonadFix m)
  => Event t b -- ^ input event, triggered in sequence
  -> m (Event t a) -- ^ final event we want to listen to


playReflex :: forall a. (Show a)
  => (forall t m. TestApp t m () a)
  -> IO (Maybe a)
playReflex network = do
  rs <- playReflexSeq [()] network
  return $ case nonEmpty rs of
    Nothing -> Nothing
    Just xs -> last xs

-- | runs a sequence of events and returns list of output event values at each point in the sequence
playReflexSeq ::
  forall b a. (Show a)
  => [b] -- ^ sequence of events to trigger
  -> (forall t m. TestApp t m b a) -- ^ entry point into reflex app
  -> IO [Maybe a]
playReflexSeq bs network =
  runSpiderHost $ do
    (tickEvent,  tickTriggerRef)  <- newEventWithTriggerRef
    tickEventHandle <- subscribeEvent tickEvent

    finalEvent <- runHostFrame $ network tickEvent
    finalEventHandle <- subscribeEvent finalEvent


    forM bs $ \b -> do
      -- you could just use this instead
      --fireEventRef
      trig <- readIORef tickTriggerRef
      let
        eventValue :: (MonadReadEvent t m, Show a) => EventHandle t a -> m (Maybe a)
        eventValue = readEvent >=> T.sequenceA
      final <- case trig of
        Nothing -> error "no trigger ref"
        Just t  -> fireEventsAndRead [t :=> Identity b] $ eventValue finalEventHandle
      case final of
        Nothing -> return Nothing
        Just x  -> return (Just x)
