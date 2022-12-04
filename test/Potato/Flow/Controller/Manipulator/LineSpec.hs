{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LineSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Common




basic_line_test :: Test
basic_line_test = assertGoatTesterWithOwlPFState emptyOwlPFState basic_line_test_m where
  basic_line_test_m = do
    verifyOwlCount 0
    -- TODO rest of test

spec :: Spec
spec = do
  describe "Line" $ do
    fromHUnitTest $ basic_line_test
