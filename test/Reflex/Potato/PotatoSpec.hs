{-# LANGUAGE RecursiveDo #-}

module Reflex.Potato.PotatoSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)

import           Reflex
import           Reflex.Potato.Helpers

import           Reflex.Host.Basic

repeatEvent_network :: forall t m. BasicGuestConstraints t m => Event t [Int] -> BasicGuest t m (Event t Int)
repeatEvent_network = repeatEvent

test_repeatEvent :: Test
test_repeatEvent = TestLabel "basic" $ TestCase $ do
  let
    bs = [[1..10],[0],[],[1..5],[],[],[1,2]] :: [[Int]]
    run :: IO [[Maybe Int]]
    run = basicHostWithStaticEvents bs repeatEvent_network
  v <- liftIO run
  --print v
  return ()
  L.last v @?= [Just 1, Just 2]

spec :: Spec
spec = do
  describe "Potato" $ do
    fromHUnitTest test_repeatEvent
