{-# LANGUAGE RecordWildCards #-}

module Reflex.Test.App (
  AppIn(..)
  , AppOut(..)
  , AppFrame(..)
  , getAppFrame
  , tickAppFrame
  , runAppSimple
) where

import           Prelude

import           Control.Monad
import           Control.Monad.Ref
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Maybe            (fromJust)
import           Data.These

import           Reflex
import           Reflex.Host.Class

data AppIn t b e = AppIn
  { _appIn_behavior :: Behavior t b
  , _appIn_event    :: Event t e
  }

data AppOut t b e = AppOut
  { _appOut_behavior :: Behavior t b
  , _appOut_event    :: Event t e
  }

data AppFrame t bIn eIn bOut eOut m = AppFrame {
  _appFrame_readPhase :: ReadPhase m (bOut, Maybe eOut)
  , _appFrame_pulseB :: EventTrigger t bIn
  , _appFrame_pulseE :: EventTrigger t eIn
  , _appFrame_fire    :: forall a. [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m [a]
}

getAppFrame ::  forall t bIn eIn bOut eOut m.
            (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
            => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
            -> bIn
            -> m (AppFrame t bIn eIn bOut eOut m)
getAppFrame app b0 = do
  (appInHoldE, pulseHoldTriggerRef) <- newEventWithTriggerRef
  (appInE, pulseEventTriggerRef) <- newEventWithTriggerRef
  appInB <- hold b0 appInHoldE
  (out :: AppOut t bOut eOut, FireCommand fire) <- hostPerformEventT $ app $ AppIn
    { _appIn_event = appInE
    , _appIn_behavior = appInB
    }
  hnd :: EventHandle t eOut <- subscribeEvent (_appOut_event out)
  mpulseB <- readRef pulseHoldTriggerRef
  mpulseE <- readRef pulseEventTriggerRef
  let readPhase = do
        b <- sample (_appOut_behavior out)
        frames <- sequence =<< readEvent hnd
        return (b, frames)
  return
    AppFrame {
      _appFrame_readPhase = readPhase
      , _appFrame_pulseB = fromJust mpulseB
      , _appFrame_pulseE = fromJust mpulseE
      , _appFrame_fire    = fire
    }

tickAppFrame :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => AppFrame t bIn eIn bOut eOut m
       -> Maybe (These bIn eIn)
       -> m [(bOut, Maybe eOut)]
tickAppFrame AppFrame {..} input = case input of
  Nothing -> fire [] $ readPhase
  Just i -> case i of
    This b' -> fire [ pulseB :=> Identity b' ] $ readPhase
    That e' -> fire [ pulseE :=> Identity e' ] $ readPhase
    These b' e' -> fire [ pulseB :=> Identity b', pulseE :=> Identity e' ] $ readPhase
  where
    fire = _appFrame_fire
    readPhase = _appFrame_readPhase
    pulseB = _appFrame_pulseB
    pulseE = _appFrame_pulseE

runApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
       -> bIn
       -> [Maybe (These bIn eIn)]
       -> IO [[(bOut, Maybe eOut)]]
runApp app b0 input = runSpiderHost $ do
  appFrame <- getAppFrame app b0
  forM input $ tickAppFrame appFrame

runAppSimple :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
             => (Event t eIn -> PerformEventT t m (Event t eOut))
             -> [eIn]
             -> IO [[Maybe eOut]]
runAppSimple app input = runApp' app (map Just input)

-- TODO rename stuff below here
runApp' :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Event t eOut))
        -> [Maybe eIn]
        -> IO [[Maybe eOut]]
runApp' app input = do
  let app' = fmap (AppOut (pure ())) . app
  map (map snd) <$> runApp (app' . _appIn_event) () (map (fmap That) input)

runAppB :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Behavior t bOut))
        -> [Maybe eIn]
        -> IO [[bOut]]
runAppB app input = do
  let app' = fmap (flip AppOut never) . app
  map (map fst) <$> runApp (app' . _appIn_event) () (map (fmap That) input)
