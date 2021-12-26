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

import           Potato.Flow.Common

import           Data.Default
import           Data.Dependent.Sum                         (DSum ((:=>)))
import qualified Data.IntMap                                as IM
import qualified Potato.Data.Text.Zipper                           as TZ

testText1 :: Text
testText1 = "aoeu\nhi\n12345wrapping"

testSBoxWithText1 :: SBox
testSBoxWithText1 = def {
    _sBox_box = LBox (V2 5 5) (V2 5 10)
    , _sBox_text = SBoxText testText1 def
    , _sBox_boxType = SBoxType_NoBoxText
  }

testClick :: Int -> Int -> RelMouseDrag
testClick x y = RelMouseDrag $ def {
    _mouseDrag_to = V2 x y
  }

boxTextInputState_basic_test :: Spec
boxTextInputState_basic_test = let
    tais1 = makeBoxTextInputState 0 testSBoxWithText1 (testClick 5 5)
    tais2 = mouseText tais1 testSBoxWithText1 (testClick 6 5) (getBoxTextOffset testSBoxWithText1)
  in
    it "makeBoxTextInputState_basic" $ do
      --traceShow tais1 $ traceShow tais2 $ 1 `shouldBe` 1
      _boxTextInputState_original tais1 `shouldBe` Just testText1
      _boxTextInputState_original tais2 `shouldBe` Just testText1
      -- TZ has no Eq instance but show works fine, whatever
      show @[Char] (TZ.right (_boxTextInputState_zipper tais1)) `shouldBe` show (_boxTextInputState_zipper tais2)


checkSBoxText :: Text -> Text -> EverythingPredicate
checkSBoxText label text = firstSuperOwlPredicate (Just label) $ \sowl -> case hasOwlElt_toSElt_hack sowl of
  SEltBox (SBox _ _ _ (SBoxText {..}) _) -> _sBoxText_text == text
  _                                         -> False

checkSBoxLabel :: Text -> Text -> EverythingPredicate
checkSBoxLabel label text = firstSuperOwlPredicate (Just label) $ \sowl -> case hasOwlElt_toSElt_hack sowl of
  SEltBox sbox -> _sBoxTitle_title (_sBox_title sbox) == Just text
  _                                         -> False

test_basic :: Test
test_basic = constructTest "basic" emptyOwlPFState bs expected where
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
      , EWCMouse (LMouseData (V2 12 11) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 12 11) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'a') [])

      , EWCLabel "exit BoxText"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "select <text> at end of line"
      , EWCMouse (LMouseData (V2 11 18) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 18) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'b') [])

      , EWCLabel "set noborder"
      , EWCSetParams $ IM.singleton 1 (CTagBoxType :=> Identity (CBoxType (SBoxType_BoxText, SBoxType_NoBoxText)))
      -- there is no border so click location is offset by (-1,-1) from before
      , EWCMouse (LMouseData (V2 11 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 10) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char '🥔') [])

    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
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

      , Combine [
          LabelCheck "set noborder"
          -- make sure REltId is 0 because next step we will modify using it
          , firstSuperOwlPredicate (Just "<text>") $ \sowl -> _superOwl_id sowl == 1
        ]
      , firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox _ _ _ _ boxtype) -> boxtype == SBoxType_NoBoxText
        _                                 -> False
      , checkHandlerNameAndState handlerName_boxText True
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "p🥔aoopb"
    ]

test_handler_state :: Test
test_handler_state = constructTest "handler state" emptyOwlPFState bs expected where
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
          firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
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

-- same test as basic except box is inverted
test_negative :: Test
test_negative = constructTest "negative" emptyOwlPFState bs expected where
  bs = [
      EWCLabel "create <text>"
      , EWCTool Tool_Text
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 0) True MouseButton_Left [] False)

      , EWCLabel "modify <text>"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'g') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])

      , EWCLabel "move cursor <text>"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 2 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 2 1) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'a') [])


    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
            -- old non-canonical version, keeping here in case we ever decide to go back to non-canonical version
            --SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 (-10) (-10))
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 0 0) (V2 (10) (10))
            _                           -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
        ]

      , LabelCheck "modify <text>"
      , checkHandlerNameAndState handlerName_boxText False
      , checkHandlerNameAndState handlerName_boxText False
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "goop"

      , LabelCheck "move cursor <text>"
      , EqPredicate _goatState_selectedTool Tool_Select
      , checkHandlerNameAndState handlerName_boxText True
      , checkHandlerNameAndState handlerName_boxText False
      , checkSBoxText "<text>" "gaoop"

    ]

test_zero :: Test
test_zero = constructTest "zero" emptyOwlPFState bs expected where
  bs = [
      EWCLabel "create <text>"
      , EWCTool Tool_Text
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 12 12) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 12 12) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char '🥔') [])

      , EWCLabel "exit BoxText"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "resize box to 0"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 12 12) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 2 2)
            _                           -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
        ]
      , checkSBoxText "<text>" "🥔"

      , LabelCheck "exit BoxText"
      , checkHandlerNameAndState handlerName_box False

      , LabelCheck "resize box to 0"
      , EqPredicate _goatState_selectedTool Tool_Select
      , AlwaysPass
      , AlwaysPass
      , firstSuperOwlPredicate (Just "<text>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 0 0)
        _                           -> False

    ]


lookup :: Int -> [a] -> Maybe a
lookup _ []       = Nothing
lookup 0 (x : _)  = Just x
lookup i (_ : xs) = lookup (i - 1) xs

-- | check the position of the cursor
checkRenderHandlerPos :: XY -> EverythingPredicate
checkRenderHandlerPos pos = FunctionPredicate $ \gs ->
  let
    h = _goatState_handler gs
    phi = potatoHandlerInputFromGoatState gs
    HandlerRenderOutput hs = pRenderHandler h phi
  -- cursor is always 4th position in HandlerRenderOutput
  in case lookup 4 hs of
    Nothing -> ("could not find render handler for " <> pHandlerName h <> " got: " <> show hs, False)
    Just (RenderHandle (LBox p _) _ _) -> if p == pos
      then ("", True)
      else ("handler output mismatch expected: " <> show pos <> " got: " <> show p, False)

test_output :: Test
test_output = constructTest "output" emptyOwlPFState bs expected where
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
      , EWCMouse (LMouseData (V2 12 11) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 12 11) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'a') [])

      , EWCLabel "exit BoxText"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "select <text> at end of line"
      , EWCMouse (LMouseData (V2 11 18) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 18) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'b') [])

      , EWCLabel "navigate"
      , EWCKeyboard (KeyboardData KeyboardKey_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Home [])
      , EWCKeyboard (KeyboardData KeyboardKey_Right [])
      , EWCKeyboard (KeyboardData KeyboardKey_Right [])
      , EWCKeyboard (KeyboardData KeyboardKey_End [])

      , EWCLabel "set noborder"
      , EWCSetParams $ IM.singleton 1 (CTagBoxType :=> Identity (CBoxType (SBoxType_BoxText, SBoxType_NoBoxText)))
      , EWCKeyboard (KeyboardData KeyboardKey_Backspace [])

      , EWCLabel "align right"
      , EWCSetParams $ IM.singleton 1 (CTagBoxTextStyle :=> Identity (CTextStyle $ DeltaTextStyle (TextStyle TextAlign_Left, TextStyle TextAlign_Right)))

    ]
  expected = [
      LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          numSelectedEltsEqualPredicate 1
          , checkRenderHandlerPos (V2 11 11)
        ]

      , LabelCheck "modify <text>"
      , checkRenderHandlerPos (V2 12 11)
      , checkRenderHandlerPos (V2 13 11)
      , checkRenderHandlerPos (V2 14 11)
      , checkRenderHandlerPos (V2 15 11)

      , LabelCheck "move cursor <text>"
      , EqPredicate _goatState_selectedTool Tool_Select
      , checkHandlerNameAndState handlerName_boxText True
      , checkRenderHandlerPos (V2 12 11)
      , checkRenderHandlerPos (V2 13 11)

      , LabelCheck "exit BoxText"
      , checkHandlerNameAndState handlerName_box False

      , LabelCheck "select <text> at end of line"
      , checkHandlerNameAndState handlerName_box True
      , checkRenderHandlerPos (V2 16 11)
      , checkRenderHandlerPos (V2 17 11)

      , LabelCheck "navigate"
      , checkRenderHandlerPos (V2 16 11)
      , checkRenderHandlerPos (V2 15 11)
      , checkRenderHandlerPos (V2 11 11)
      , checkRenderHandlerPos (V2 12 11)
      , checkRenderHandlerPos (V2 13 11)
      , checkRenderHandlerPos (V2 17 11)


      , Combine [
          LabelCheck "set noborder"
          -- make sure REltId is 0 because next step we will modify using it
          , firstSuperOwlPredicate (Just "<text>") $ \sowl -> _superOwl_id sowl == 1
        ]
      , checkRenderHandlerPos (V2 16 10)
      , checkRenderHandlerPos (V2 15 10)

      , Combine [
          LabelCheck "align right"
          -- make sure REltId is 0 because next step we will modify using it
          , firstSuperOwlPredicate (Just "<text>") $ \sowl -> _superOwl_id sowl == 1
        ]
      , checkRenderHandlerPos (V2 19 10)
    ]



test_boxlabel_basic :: Test
test_boxlabel_basic = constructTest "basic" emptyOwlPFState bs expected where
  bs = [
      EWCLabel "create <box>"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "select <box> label"
      , EWCMouse (LMouseData (V2 12 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 12 10) True MouseButton_Left [] False)

      , EWCLabel "modify <box> label"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'o') [])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'p') [])
    ]
  expected = [
      LabelCheck "create <box>"
      , EqPredicate _goatState_selectedTool Tool_Box
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<box>") $ \sowl -> case hasOwlElt_toSElt_hack sowl of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 10 10)
            _                           -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_box False
        ]

      , LabelCheck "select <box> label"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_boxLabel False

      , LabelCheck "modify <box> label"
      , checkHandlerNameAndState handlerName_boxLabel False
      , checkHandlerNameAndState handlerName_boxLabel False
      , checkHandlerNameAndState handlerName_boxLabel False
      , checkSBoxLabel "<box>" "poop"
    ]

spec :: Spec
spec = do
  describe "BoxTextHandel" $ do
    boxTextInputState_basic_test
    fromHUnitTest $ test_basic
    fromHUnitTest $ test_handler_state
    fromHUnitTest $ test_negative
    fromHUnitTest $ test_zero
    fromHUnitTest $ test_output
  describe "BoxLabelHandler" $ do
    fromHUnitTest $ test_boxlabel_basic
