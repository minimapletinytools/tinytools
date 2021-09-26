{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.ManipulatorSpec
  ( spec
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow

import           Potato.Flow.Common
import           Potato.Flow.TestStates


test_BoxHandler_drag :: Test
test_BoxHandler_drag = constructTest "drag" owlpfstate_basic1 bs expected where
  bs = [
      EWCTool Tool_Select

      , EWCLabel "select b2"
      -- Note, this is a little weird, even though we are just selecting (no dragging) it will still go to BoxHandler first to do the selection...
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "resize tl corner b2"
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "resize tr corner b2"
      , EWCMouse (LMouseData (V2 15 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 15 8) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 15 8) True MouseButton_Left [] False)

      , EWCLabel "resize bl corner b2"
      , EWCMouse (LMouseData (V2 10 15) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 7 15) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 7 15) True MouseButton_Left [] False)

      , EWCLabel "resize br corner b2"
      , EWCMouse (LMouseData (V2 15 15) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "area move b2"
      , EWCMouse (LMouseData (V2 15 15) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)
    ]
  expected = [
      EqPredicate _goatState_selectedTool Tool_Select

      , LabelCheck "select b2"
      , checkHandlerNameAndState handlerName_box True
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "resize tl corner b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 11 11) (V2 4 4)
        _                           -> False

      , LabelCheck "resize tr corner b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 11 9) (V2 4 6)
        _                           -> False

      , LabelCheck "resize bl corner b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 8 9) (V2 7 6)
        _                           -> False

      , LabelCheck "resize br corner b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 8 9) (V2 12 11)
        _                           -> False

      , LabelCheck "area move b2"
      , AlwaysPass
      , AlwaysPass
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 13 14) (V2 12 11)
        _                           -> False
    ]

-- TODO test this on non-SBox stuff too
test_BoxHandler_select_and_drag :: Test
test_BoxHandler_select_and_drag = constructTest "select and drag" owlpfstate_basic1 bs expected where
  bs = [
      EWCTool Tool_Select

      , EWCLabel "select + drag b2"
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 11) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 11 11) True MouseButton_Left [] False)

    ]
  expected = [
      EqPredicate _goatState_selectedTool Tool_Select

      , LabelCheck "select + drag b2"
      , checkHandlerNameAndState handlerName_box True
      , numSelectedEltsEqualPredicate 1
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 11 11) (V2 5 5)
        _                           -> False

    ]


-- TODO
--test_BoxHandler_boundingbox

test_BoxHandler_restrict8 :: Test
test_BoxHandler_restrict8 = constructTest "restrict8" owlpfstate_basic1 bs expected where
  bs = [
      EWCTool Tool_Select

      , EWCLabel "select b2"
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "resize tl corner b2 while holding shift"
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 0) False MouseButton_Left [KeyModifier_Shift] False)
      , EWCMouse (LMouseData (V2 10 0) True MouseButton_Left [KeyModifier_Shift] False)
    ]
  expected = [
      EqPredicate _goatState_selectedTool Tool_Select

      , LabelCheck "select b2"
      , checkHandlerNameAndState handlerName_box True
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "resize tl corner b2 while holding shift"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , firstSuperOwlPredicate (Just "b2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == 10 && y == 1
        _                                        -> False
    ]

test_BoxHandler_clickOnSelectionDragging :: Test
test_BoxHandler_clickOnSelectionDragging = constructTest "drags only when click on selection" owlpfstate_basic1 bs expected where
  bs = [
      EWCTool Tool_Select

      , EWCLabel "select all boxes"
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "click on something and drag"
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] False)

      , EWCLabel "click on nothing and drag"
      , EWCMouse (LMouseData (V2 8 8) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
    ]
  expected = [
      EqPredicate _goatState_selectedTool Tool_Select

      , LabelCheck "select all boxes"
      , checkHandlerNameAndState handlerName_select True
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate 4

      , LabelCheck "click on something and drag"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , firstSuperOwlPredicate (Just "b1") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == 1 && y == 1
        _                                        -> False

      , LabelCheck "click on nothing and drag"
      , checkHandlerNameAndState handlerName_select True
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate 0
    ]

test_LineHandler_drag :: Test
test_LineHandler_drag = constructTest "drag" owlpfstate_basic1 bs expected where
  bs = [
      EWCTool Tool_Select

      , EWCLabel "select sl1"
      , EWCMouse (LMouseData (V2 0 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 100) True MouseButton_Left [] False)

      , EWCLabel "move end of line"
      , EWCMouse (LMouseData (V2 0 110) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 120) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 120) True MouseButton_Left [] False)

      , EWCLabel "select sl2"
      , EWCMouse (LMouseData (V2 2 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 2 100) True MouseButton_Left [] False)

      , EWCLabel "move start of line"
      , EWCMouse (LMouseData (V2 0 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 90) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 90) True MouseButton_Left [] False)
    ]
  expected = [
      EqPredicate _goatState_selectedTool Tool_Select

      , LabelCheck "select sl1"
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_simpleLine False
        ]

      , LabelCheck "move end of line"
      , checkHandlerNameAndState handlerName_simpleLine True
      , checkHandlerNameAndState handlerName_simpleLine True
      , firstSuperOwlPredicate (Just "sl1") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltLine (SSimpleLine start end _ _) -> start == (V2 0 100) && end == (V2 0 120)
        _                                  -> False

      , LabelCheck "select sl2"
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_simpleLine False
        ]

      , LabelCheck "move start of line"
      , checkHandlerNameAndState handlerName_simpleLine True
      , checkHandlerNameAndState handlerName_simpleLine True
      , firstSuperOwlPredicate (Just "sl2") $ \sowl -> case isOwl_toSElt_hack sowl of
        SEltLine (SSimpleLine start end _ _) -> start == (V2 0 90) && end == (V2 10 100)
        _                                  -> False
    ]


-- this should work with any initial state so long as default names aren't used
test_Common_create :: Test
test_Common_create = constructTest "create" owlpfstate_basic1 bs expected where
  bs = [

      EWCLabel "create <box>"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "create <line>"
      , EWCTool Tool_Line
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 20) True MouseButton_Left [] False)

      , EWCLabel "create <text>"
      , EWCTool Tool_Text
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 120 120) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 120 120) True MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
    ]
  expected = [
      LabelCheck "create <box>"
      , EqPredicate _goatState_selectedTool Tool_Box
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<box>") $ \sowl -> case isOwl_toSElt_hack sowl of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 10 10) (V2 10 10)
            _                           -> False
          , numSelectedEltsEqualPredicate 1
        ]

      , LabelCheck "create <line>"
      , EqPredicate _goatState_selectedTool Tool_Line
      , checkHandlerNameAndState handlerName_simpleLine True
      , checkHandlerNameAndState handlerName_simpleLine True
      , Combine [
          firstSuperOwlPredicate (Just "<line>") $ \sowl -> case isOwl_toSElt_hack sowl of
            SEltLine (SSimpleLine start end _ _) -> start == (V2 10 10) && end == (V2 20 20)
            _                    -> False
          , numSelectedEltsEqualPredicate 1
        ]

      , LabelCheck "create <text>"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          firstSuperOwlPredicate (Just "<text>") $ \sowl -> case isOwl_toSElt_hack sowl of
            SEltBox (SBox lbox _ _ _ _) -> lbox == LBox (V2 100 100) (V2 20 20)
            _                         -> False
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
        ]
      , checkHandlerNameAndState handlerName_box False



    ]

spec :: Spec
spec = do
  describe "Manipulator" $ do
    describe "BoxHandler" $ do
      fromHUnitTest $ test_BoxHandler_drag
      fromHUnitTest $ test_BoxHandler_select_and_drag
      fromHUnitTest $ test_BoxHandler_restrict8
      fromHUnitTest $ test_BoxHandler_clickOnSelectionDragging
    describe "LineHandler" $ do
      fromHUnitTest $ test_LineHandler_drag
    describe "Common" $ do
      fromHUnitTest $ test_Common_create
