{-# LANGUAGE RecordWildCards #-}

module Reflex.Test.AppSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.App

import qualified Data.List                as L
import           Data.These



basic_network :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t Int Int -> PerformEventT t m (AppOut t Int Int))
basic_network AppIn {..} = return
  AppOut {
    _appOut_behavior = fmap (*(-1)) _appIn_behavior
    , _appOut_event = fmap (\(b,e) -> e+b) $ attach _appIn_behavior _appIn_event
  }



test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $ do
  let
    b = 10 :: Int
    es = [1..10] :: [Int]
  appFrame <- getAppFrame basic_network b
  forM_ es $ \e -> do
    out <- tickAppFrame appFrame (Just (That e))
    liftIO $ L.last out @?= (-b, Just (b+e))

spec :: Spec
spec = describe "Reflex.Test.App" $ do
  fromHUnitTest test_basic
