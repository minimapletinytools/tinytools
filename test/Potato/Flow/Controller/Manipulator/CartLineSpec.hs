{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.CartLineSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow

import           Potato.Flow.Common

import           Data.Default
import           Data.Dependent.Sum                         (DSum ((:=>)))
import qualified Data.IntMap                                as IM
import qualified Potato.Data.Text.Zipper                           as TZ

test_basic :: Test
test_basic = constructTest "basic" emptyOwlPFState bs expected where
  bs = [
      EWCLabel "create <line>"
      , EWCTool Tool_Line
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
    ]
  expected = [
      LabelCheck "create <line>"
      , EqPredicate _goatState_selectedTool Tool_Line
      , checkHandlerNameAndState handlerName_simpleLine True
      , Combine [
          firstSuperOwlPredicate (Just "<line>") $ \sowl -> case isOwl_toSElt_hack sowl of
            _ -> True
            -- TODO
            --SEltLine (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 10 10)
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_cartesianLine False
        ]
    ]


spec :: Spec
spec = do
  describe "CartLine" $ do
    fromHUnitTest $ test_basic
