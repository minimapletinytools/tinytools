{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.BoxTextSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.BoxText

import           Potato.Flow.Common

import           Data.Default
import qualified Data.Text.Zipper                           as TZ

testText1 :: Text
testText1 = "aoeu\nhi\n12345wrapping"

testSBoxWithText1 :: SBox
testSBoxWithText1 = def {
    _sBox_box = LBox (V2 5 5) (V2 5 10)
    , _sBox_text = SBoxText testText1 def
    , _sBox_isTextBox = True
  }

testClick :: Int -> Int -> RelMouseDrag
testClick x y = RelMouseDrag $ def {
    _mouseDrag_to = V2 x y
  }

makeBoxTextInputState_basic_test :: Spec
makeBoxTextInputState_basic_test = let
    tais1 = makeBoxTextInputState testSBoxWithText1 (testClick 5 5)
    tais2 = mouseText (Just tais1) testSBoxWithText1 (testClick 6 5)
  in
    it "makeBoxTextInputState_basic" $ do
      --traceShow tais1 $ traceShow tais2 $ 1 `shouldBe` 1
      _boxTextInputState_original tais1 `shouldBe` testText1
      _boxTextInputState_original tais2 `shouldBe` testText1
      -- TZ has no Eq instance but show works fine, whatever
      show (TZ.right (_boxTextInputState_zipper tais1)) `shouldBe` show (_boxTextInputState_zipper tais2)


checkSBoxText :: Text -> Text -> EverythingPredicate
checkSBoxText label text = firstSuperSEltLabelPredicate (Just label) $ \(_,_,SEltLabel _ selt) -> case selt of
  SEltBox (SBox lbox _ _ (SBoxText {..}) _) -> _sBoxText_text == text
  _                                         -> False

test_basic :: Test
test_basic = constructTest "basic" emptyPFState bs expected where
  bs = [
      EWCLabel "create <text>"
      , EWCTool Tool_Text
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)



      , EWCLabel "modify <text>"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])

      , EWCLabel "move cursor <text>"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 11 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 10) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'a') [])

      , EWCLabel "exit BoxText"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "select <text> at end of line"
      , EWCMouse (LMouseData (V2 10 18) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 18) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'b') [])

    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperSEltLabelPredicate (Just "<text>") $ \(_,_,SEltLabel _ selt) -> case selt of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 10 10)
            _                           -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
        ]

      , LabelCheck "modify <text>"
      , checkHandlerNameAndState handlerName_boxText False
      , checkHandlerNameAndState handlerName_boxText False
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "poop"

      , LabelCheck "move cursor <text>"
      , EqPredicate _goatState_selectedTool Tool_Select
      , checkHandlerNameAndState handlerName_boxText True
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "paoop"

      , LabelCheck "exit BoxText"
      , checkHandlerNameAndState handlerName_box False

      , LabelCheck "select <text> at end of line"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "paoopb"
    ]

test_handler_state :: Test
test_handler_state = constructTest "handler state" emptyPFState bs expected where
  bs = [
      EWCLabel "create <text>"
      , EWCTool Tool_Text
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "exit BoxText"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "deselect <text>"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 0) True MouseButton_Left [] False)

      , EWCLabel "select + drag <text>"
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 11) True MouseButton_Left [] False)

      , EWCLabel "enter edit mode"
      , EWCMouse (LMouseData (V2 11 15) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 15) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char '😱') [])

    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperSEltLabelPredicate (Just "<text>") $ \(_,_,SEltLabel _ selt) -> case selt of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 10 10)
            _                           -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
        ]

      , LabelCheck "exit BoxText"
      , checkHandlerNameAndState handlerName_box False

      , LabelCheck "deselect <text>"
      , EqPredicate _goatState_selectedTool Tool_Select
      , checkHandlerNameAndState handlerName_select True
      , Combine [
          checkHandlerNameAndState handlerName_empty False
          , numSelectedEltsEqualPredicate 0
        ]

      , LabelCheck "select + drag <text>"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box False

      , LabelCheck "enter edit mode"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "😱"

    ]

spec :: Spec
spec = do
  describe "BoxText" $ do
    makeBoxTextInputState_basic_test
    fromHUnitTest $ test_basic
    fromHUnitTest $ test_handler_state
