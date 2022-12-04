{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Potato.Flow.Deprecated.Controller.Manipulator.TextAreaSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow

import           Potato.Flow.Common

import qualified Data.Map                                as Map

checkSTextAreaTextAt :: Text -> XY -> PChar -> EverythingPredicate
checkSTextAreaTextAt label k c = firstSuperOwlPredicate (Just label) $ \sowl -> case hasOwlItem_toSElt_hack sowl of
  SEltTextArea (STextArea _ tm _) -> Map.lookup k tm == Just c
  _                                         -> False

test_basic :: Test
test_basic = constructTest "basic" emptyOwlPFState bs expected where
  bs = [
      -- TODO  text area tool???
      EWCLabel "create <text>"
      , EWCTool Tool_Text
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "modify <text>"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      --, EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])

    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlItem_toSElt_hack sowl of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 10 10)
            _                           -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
        ]

      , LabelCheck "modify <text>"
      , checkHandlerNameAndState handlerName_boxText False
      , checkHandlerNameAndState handlerName_boxText False
      , checkHandlerNameAndState handlerName_boxText False
      --, checkSBoxText "<text>" "poop"

    ]



spec :: Spec
spec = do
  describe "TextAreaHandler" $ do
    fromHUnitTest $ test_basic
