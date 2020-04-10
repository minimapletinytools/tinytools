module Reflex.TestHarnessSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex.TestHarness

basic_run :: IO (Maybe Text)
basic_run = playReflex network where
  network ev = return (fmap (const "meow") ev)

basic_test :: Test
basic_test = TestLabel "cat goes meow" $ TestCase $ do
  v <- liftIO basic_run
  Just "meow" @?= v

spec :: Spec
spec = describe "TestHarness" $ do
  fromHUnitTest basic_test
