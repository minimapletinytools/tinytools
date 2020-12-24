{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.EverythingWidgetSpec
  ( spec
  )
where

import           Relude                                  hiding (empty,
                                                          fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.Controller.Everything
import           Potato.Flow.Controller.EverythingWidget
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input

-- test imports
import           Potato.Flow.Common
import           Potato.Flow.TestStates

import qualified Data.IntMap                             as IM
import qualified Data.Sequence                           as Seq

someState1 :: PFState
someState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, someSEltLabel), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel), (5, folderEnd)]
      , _pFState_canvas = someSCanvas
  }

-- simple bespoke testing
tool_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Tool -> TestGuestT t m (Event t Tool))
tool_network ev = do
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig { _everythingWidgetConfig_selectTool = ev }
  return $ updated . _everythingWidget_tool $ everythingWidget

tool_test :: Test
tool_test = TestLabel "tool" $ TestCase $ do
  let
    -- note, starting value is TSelect
    bs = [Tool_Pan, Tool_Select, Tool_Pan, Tool_Pan, Tool_Box, Tool_Line, Tool_Text]
    expected = [Just Tool_Pan, Just Tool_Select, Just Tool_Pan, Nothing, Just Tool_Box, Just Tool_Line, Just Tool_Text]
    run = runAppSimple tool_network bs
  v <- liftIO run
  (join v) @?= expected

select_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t (Bool, Selection) -> TestGuestT t m (Event t Selection)
select_network ev = do
  let
    addSelectEv = fmapMaybe (\(b,s) -> if b then Just s else Nothing) ev
    newSelectEv = fmapMaybe (\(b,s) -> if not b then Just s else Nothing) ev
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig {
      _everythingWidgetConfig_initialState = someState1
      , _everythingWidgetConfig_selectNew = newSelectEv
      , _everythingWidgetConfig_selectAdd = addSelectEv
    }
  return $ updated . _everythingWidget_selection $ everythingWidget

select_test :: Test
select_test = TestLabel "select" $ TestCase $ do
  let
    mySelection1 = Seq.fromList [(1,1,someSEltLabel)]
    mySelection2 = Seq.fromList [(2,2,someSEltLabel)]
    combined = Seq.fromList [(1,1,someSEltLabel), (2,2,someSEltLabel)]
    bs = [(False, mySelection1), (True, mySelection2), (True, mySelection1), (False, mySelection1), (True, Seq.empty), (False, Seq.empty)]
    expected = [Just mySelection1, Just combined, Just mySelection2, Just mySelection1, Nothing, Just (Seq.empty)]
    run = runAppSimple select_network bs
  v <- liftIO run
  (join v) @?= expected

expectState :: PFState -> EverythingPredicate
expectState pfs = FunctionPredicate $
  (\pfs' -> ("expected pfstate_basic1", pfs == pfs'))
  . _everythingCombined_pFState

everything_load_test :: Test
everything_load_test = constructTest "load" emptyPFState bs expected where
  bs = [
      EWCLabel "Load"
      , EWCLoad (pFState_to_sPotatoFlow pfstate_basic1, emptyControllerMeta)
      , EWCLoad (pFState_to_sPotatoFlow pfstate_basic1, emptyControllerMeta)
      , EWCLoad (pFState_to_sPotatoFlow pfstate_someValidState1, emptyControllerMeta)
      , EWCLoad (pFState_to_sPotatoFlow pfstate_basic1, emptyControllerMeta)
      , EWCLoad (pFState_to_sPotatoFlow pfstate_someValidState1, emptyControllerMeta)
      , EWCLoad (pFState_to_sPotatoFlow emptyPFState, emptyControllerMeta)
    ]
  expected = [
      LabelCheck "Load"
      , expectState pfstate_basic1
      , expectState pfstate_basic1
      , expectState pfstate_someValidState1
      , Combine [
          expectState pfstate_basic1
          -- intersects "b1" "b2" "b3" "b4" in pfstate_basic1
          , numEltsInLBoxUsingBroadphasePredicate 4 (LBox 0 (V2 15 15))
        ]
      , expectState pfstate_someValidState1
      , Combine [
          expectState emptyPFState
          , numEltsInLBoxUsingBroadphasePredicate 0 (LBox 0 (V2 1000 1000))
        ]


    ]


everything_basic_test :: Test
everything_basic_test = constructTest "basic" emptyPFState bs expected where
  bs = [
      -- test basic panning
      EWCLabel "Pan"
      , EWCTool Tool_Pan
      -- drag to (1, 1) and release
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [])
      -- drag to (10, 15) and cancel without releasing
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 9 14) False MouseButton_Left [])
      -- cancel and keep tracking and make sure nothing changes
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 9 100) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 (-20) 31) True MouseButton_Left [])

      -- create elt A
      , EWCLabel "Create A"
      , EWCTool Tool_Box
      -- drag from (1,1) to (10,10) and release (actually (2,2) to (10,10))
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])
      , EWCNothing -- dummy to check state

      , EWCLabel "create another elt, but cancel it"
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])

      , EWCLabel "press escape a bunch of times and make sure nothing breaks"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "create elt B"
      , EWCMouse (LMouseData (V2 0 20) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 20 30) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])
      , EWCNothing -- dummy to check state

      -- unselect
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [])

      , EWCLabel "single click select elt B"
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [])

      -- now select elts A + B
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [])

      , EWCLabel "begin selecting nothing and cancel"
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 200 200) False MouseButton_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 200 200) True MouseButton_Left [])

      , EWCLabel "single click shift unselect elt B"
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [KeyModifier_Shift])
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [KeyModifier_Shift])

      , EWCLabel "unselect"
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [])

      , EWCLabel "single click select elt A"
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [])

      , EWCLabel "manipulate A"
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 (-1) (-1)) True MouseButton_Left [])

      , EWCLabel "single click shift select elt B"
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [KeyModifier_Shift])
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [KeyModifier_Shift])

      , EWCLabel "manipulate A+B"
      , EWCMouse (LMouseData (V2 5 5) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 7 5) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 7 5) True MouseButton_Left [])

      , EWCLabel "Mainpulate A+B then cancel"
      , EWCMouse (LMouseData (V2 7 5) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 7 5) True MouseButton_Left [])


      -- TODO delete the elt
      -- check in layers and check render
    ]

  expected = [
      LabelCheck "Pan"
      , (EqPredicate _everythingCombined_selectedTool Tool_Pan)
      , (EqPredicate _everythingCombined_pan (V2 0 0))
      , (EqPredicate _everythingCombined_pan (V2 1 1))
      , (EqPredicate _everythingCombined_pan (V2 1 1))
      , (EqPredicate _everythingCombined_pan (V2 0 0))
      , (EqPredicate _everythingCombined_pan (V2 10 15))
      , (EqPredicate _everythingCombined_pan (V2 1 1))
      , AlwaysPass
      , (EqPredicate _everythingCombined_pan (V2 1 1))

      , LabelCheck "Create A"
      , (EqPredicate _everythingCombined_selectedTool Tool_Box)
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box False
      , Combine [
          PFStateFunctionPredicate (checkNumElts 1)
          , numSelectedEltsEqualPredicate 1
        ]

      , LabelCheck "create another elt, but cancel it"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box False
      , Combine [
          PFStateFunctionPredicate (checkNumElts 1) -- make sure no elt was created
          , numSelectedEltsEqualPredicate 0 -- the newly created elt gets selected and after cancelling, the previous selection is lost, womp womp
          , checkHandlerNameAndState handlerName_empty False -- handler defaults to empty selection after cancelling :(
        ]

      , LabelCheck "press escape a bunch of times and make sure nothing breaks"
      , Combine [
          numSelectedEltsEqualPredicate 0
          , checkHandlerNameAndState handlerName_empty False
        ]
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass

      , LabelCheck "create elt B"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box False
      , Combine [
          PFStateFunctionPredicate (checkNumElts 2) -- make sure second box was created
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_box False
        ]

      -- unselect
      , (EqPredicate _everythingCombined_selectedTool Tool_Select)
      , checkHandlerNameAndState handlerName_select True
      , Combine [
          numSelectedEltsEqualPredicate 0
          , checkHandlerNameAndState handlerName_empty False
        ]

      , LabelCheck "single click select elt B"
      , checkHandlerNameAndState handlerName_box True -- select+drag case, expect box handler
      , Combine [
          numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_box False
        ]

      -- now select elts A + B
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate 2

      , LabelCheck "begin selecting nothing and cancel"
      , checkHandlerNameAndState handlerName_select True
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate 2
      , numSelectedEltsEqualPredicate 2

      , LabelCheck "single click shift unselect elt B"
      , checkHandlerNameAndState handlerName_select True -- single click shift unselect case uses select handler
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "unselect"
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "single click select elt A"
      , checkHandlerNameAndState handlerName_box True
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "manipulate A"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      -- check that it got moved to 0 0
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 0 && y == 0
        _                                  -> False)

      , LabelCheck "single click shift select elt B"
      , checkHandlerNameAndState handlerName_select True -- single click shift unselect case uses select handler
      , numSelectedEltsEqualPredicate 2

      , LabelCheck "manipulate A+B"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      -- check that first elt A got moved over by 2
      -- TODO also check elt B
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 2 && y == 0
        _                                  -> False)

      , LabelCheck "Mainpulate A+B then cancel"
      , checkHandlerNameAndState handlerName_box True
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 5 && y == 5
        _                                  -> False)
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 2 && y == 0
        _                                  -> False)
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 2 && y == 0
        _                                  -> False)


    ]


spec :: Spec
spec = do
  describe "EverythingWidget" $ do
    fromHUnitTest $ tool_test
    fromHUnitTest $ select_test
    fromHUnitTest $ everything_load_test
    fromHUnitTest $ everything_basic_test
