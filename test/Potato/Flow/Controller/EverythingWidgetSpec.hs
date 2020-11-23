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
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      -- create elt A
      , EWCLabel "Create A"
      , EWCTool Tool_Box
      -- drag from (1,1) to (10,10) and release
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])
      , EWCNothing -- dummy to check state

      -- create another elt, but cancel it
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCNothing -- dummy to check state

      -- create elt B
      , EWCMouse (LMouseData (V2 0 20) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 20 30) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [])
      , EWCNothing -- dummy to check state

      -- select elt B
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [])

      -- now select elts A + B
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [])

      -- begin selecting nothing and cancel
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 200 200) False MouseButton_Left [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      -- shift unselect elt B
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [KeyModifier_Shift])

      -- unselect
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [])

      -- select elt A
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [])

      -- manipulate A
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 (-1) (-1)) True MouseButton_Left [])

      -- shift select elt B
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [KeyModifier_Shift])

      -- manipulate A+B
      , EWCMouse (LMouseData (V2 5 5) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 7 5) False MouseButton_Left [])
      , EWCMouse (LMouseData (V2 7 5) True MouseButton_Left [])

      -- TODO delete the elt
      -- check in layers and check render
    ]

  expected = [
      -- very basic panning
      LabelCheck "Pan"
      , (EqPredicate _everythingCombined_selectedTool Tool_Pan)
      , (EqPredicate _everythingCombined_pan (V2 0 0))
      , (EqPredicate _everythingCombined_pan (V2 1 1))
      , (EqPredicate _everythingCombined_pan (V2 1 1))
      , (EqPredicate _everythingCombined_pan (V2 0 0))
      , (EqPredicate _everythingCombined_pan (V2 10 15))
      , (EqPredicate _everythingCombined_pan (V2 1 1))

      -- create elt A
      , LabelCheck "Create A"
      , (EqPredicate _everythingCombined_selectedTool Tool_Box)
      , checkLastOperationPredicate LastOperationType_Manipulate
      , checkLastOperationPredicate LastOperationType_Manipulate
      , checkLastOperationPredicate LastOperationType_None
      , Combine [
          PFStateFunctionPredicate (checkNumElts 1)
          -- TODO test other things
        ]

      -- create another elt, but cancel it
      , checkLastOperationPredicate LastOperationType_Manipulate
      , checkLastOperationPredicate LastOperationType_Manipulate
      , checkLastOperationPredicate LastOperationType_Undo
      , PFStateFunctionPredicate (checkNumElts 1) -- make sure no elt was created

      -- create elt B
      , checkLastOperationPredicate LastOperationType_Manipulate
      , checkLastOperationPredicate LastOperationType_Manipulate
      , checkLastOperationPredicate LastOperationType_None
      , PFStateFunctionPredicate (checkNumElts 2) -- make sure second box was created

      -- select elt B
      , (EqPredicate _everythingCombined_selectedTool Tool_Select)
      , AlwaysPass
      , numSelectedEltsEqualPredicate 1

      -- now select elts A + B
      , AlwaysPass
      , numSelectedEltsEqualPredicate 2

      -- beging selecting nothing and cancel
      , AlwaysPass
      , AlwaysPass
      , numSelectedEltsEqualPredicate 2

      -- shift unselect elt B
      , AlwaysPass
      , numSelectedEltsEqualPredicate 1

      -- unselect
      , AlwaysPass
      , numSelectedEltsEqualPredicate 0

      -- select elt A
      , AlwaysPass
      , numSelectedEltsEqualPredicate 1

      -- manipulate A
      , checkLastOperationPredicate LastOperationType_Manipulate
      , AlwaysPass
      -- check that it got moved to 0 0
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 0 && y == 0
        _                                  -> False)

      -- shift select elt B
      , AlwaysPass
      , numSelectedEltsEqualPredicate 2

      -- manipulate A+B
      , AlwaysPass
      , AlwaysPass
      -- check that first elt A got moved over by 2
      -- TODO also check elt B
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 2 && y == 0
        _                                  -> False)

    ]


spec :: Spec
spec = do
  describe "EverythingWidget" $ do
    fromHUnitTest $ tool_test
    fromHUnitTest $ select_test
    fromHUnitTest $ everything_basic_test
