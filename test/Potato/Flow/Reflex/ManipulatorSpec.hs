{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.ManipulatorSpec
  ( spec
  )
where

import           Relude                    hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow

import           Potato.Flow.Reflex.Common
import           Potato.Flow.TestStates


basic_sbox_test :: Test
basic_sbox_test = constructTest "manipulator - sbox" emptyPFState bs expected where
  bs = [
      EWCLabel "setup"
    ]
  expected = [
      LabelCheck "setup"
    ]


spec :: Spec
spec = do
  describe "Manipulator" $ do
    fromHUnitTest $ basic_sbox_test
