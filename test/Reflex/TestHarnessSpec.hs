module Reflex.TestHarnessSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex.TestHarness


basic_test :: Test
basic_test = TestLabel "cat goes meow" $ TestCase $ do
  let
    network ev = return (fmap (const "meow") ev)
    run :: IO (Maybe Text)
    run = playReflex network where

  v <- liftIO run
  Just "meow" @?= v

basic_seq_test :: Test
basic_seq_test = TestLabel "cat goes meow meow meow" $ TestCase $ do
  let
    bs = [0..10] :: [Int]
    mapfn = (\n -> "meow" <> show n)
    network :: TestApp t m Int Text
    network ev = return (fmap mapfn ev)
    run :: IO [Maybe Text]
    run = playReflexSeq bs network
  v <- liftIO run
  fmap (Just . mapfn) bs @?= v


spec :: Spec
spec = describe "TestHarness" $ do
  fromHUnitTest basic_test
  fromHUnitTest basic_seq_test
