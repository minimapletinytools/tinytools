{-# LANGUAGE RecursiveDo #-}

module Reflex.Potato.HelpersSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)

import           Reflex
import           Reflex.Potato.Helpers

import           Reflex.Test.App


sequenceEvents_network ::
  forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t (Int,Int) -> PerformEventT t m (Event t Int))
sequenceEvents_network ev = mdo
  let
    fstEv = fmap fst ev
    sndEv = fmap snd ev
  delayedSndEv <- sequenceEvents fstEv sndEv
  return $ leftmostwarn "sequenceEvents" [fstEv, delayedSndEv]

test_sequenceEvents :: Test
test_sequenceEvents = TestLabel "sequenceEvents" $ TestCase $ do
  let
    bs = [(0,1)] :: [(Int,Int)]
    run :: IO [[Maybe Int]]
    run = runAppSimple sequenceEvents_network bs
  v <- liftIO run
  join v @?= [Just 0, Just 1]

repeatEventAndCollectOutput_network ::
  forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t [Int] -> PerformEventT t m (Event t [Int]))
repeatEventAndCollectOutput_network ev = mdo
  (repeated, collected) <- repeatEventAndCollectOutput ev repeated
  return collected

test_repeatEventAndCollectOutput :: Test
test_repeatEventAndCollectOutput = TestLabel "repeatEventAndCollectOutput" $ TestCase $ do
  let
    bs = [[0],[],[1..5],[],[],[1,2],[1..10],[]] :: [[Int]]
    run :: IO [[Maybe [Int]]]
    run = runAppSimple repeatEventAndCollectOutput_network bs
  v <- liftIO run
  fmap L.last v @?= fmap Just bs

repeatEvent_network ::
  forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t [Int] -> PerformEventT t m (Event t Int))
repeatEvent_network = repeatEvent

test_repeatEvent :: Test
test_repeatEvent = TestLabel "repeatEvent" $ TestCase $ do
  let
    bs = [[1..10],[0],[],[1..5],[],[],[1,2]] :: [[Int]]
    run :: IO [[Maybe Int]]
    run = runAppSimple repeatEvent_network bs
  v <- liftIO run
  --print v
  return ()
  L.last v @?= [Just 1, Just 2]

spec :: Spec
spec = do
  describe "Potato" $ do
    fromHUnitTest test_repeatEvent
    fromHUnitTest test_repeatEventAndCollectOutput
    fromHUnitTest test_sequenceEvents
