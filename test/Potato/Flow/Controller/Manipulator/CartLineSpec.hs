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
      EWCLabel "create <cartline>"
      , EWCTool Tool_CartLine
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 10) True MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)
      -- click on same point to finish it
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

    ]
  expected = [
      LabelCheck "create <cartline>"
      , EqPredicate _goatState_selectedTool Tool_CartLine
      , checkHandlerNameAndState handlerName_cartesianLine True
      , checkHandlerNameAndState handlerName_cartesianLine True
      , checkHandlerNameAndState handlerName_cartesianLine True
      , checkHandlerNameAndState handlerName_cartesianLine True
      , checkHandlerNameAndState handlerName_cartesianLine True
      , checkHandlerNameAndState handlerName_cartesianLine True
      , checkHandlerNameAndState handlerName_cartesianLine False
      , checkHandlerNameAndState handlerName_cartesianLine False

    ]


spec :: Spec
spec = do
  describe "CartLine" $ do
    fromHUnitTest $ test_basic
