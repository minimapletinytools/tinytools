{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.EverythingWidgetSpec
  ( spec
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.Controller.GoatWidget
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Layers

-- test imports
import           Potato.Flow.Common
import           Potato.Flow.TestStates

import qualified Data.IntMap                       as IM
import qualified Data.List.Ordered                 as L
import qualified Data.Sequence                     as Seq
import           Data.Tuple.Extra


expectState :: PFState -> EverythingPredicate
expectState pfs = FunctionPredicate $
  (\pfs' -> ("expected pfstate_basic1", pfs == pfs'))
  . goatState_pFState

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

validateLayersOrderPredicate :: EverythingPredicate
validateLayersOrderPredicate = r where
  sortingfn le1 le2 = layerEntry_layerPos le1 < layerEntry_layerPos le2
  r = FunctionPredicate $
    (\(_,lentries) -> ("expected LayerEntries in order " <> show (fmap (snd3 . _layerEntry_superSEltLabel) lentries), L.isSortedBy sortingfn (toList lentries)))
    . _goatState_layersState

checkLayerEntriesNum :: Int -> EverythingPredicate
checkLayerEntriesNum n = r where
  r = FunctionPredicate $
    (\(_,lentries) -> ("expected " <> show n <> " got " <> show (Seq.length lentries), Seq.length lentries == n))
    . _goatState_layersState

everything_layers_test :: Test
everything_layers_test = constructTest "layers" pfstate_basic1 bs expected where
  bs = [
      EWCLabel "Initial"
      , EWCNothing -- dummy to check state

      , EWCLabel "Create A"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
      , EWCNothing -- dummy to check state
    ]
  expected = [
      LabelCheck "Initial"
      -- this isn't an especially useful/exciting test... but it's better than nothing
      , Combine [
        validateLayersOrderPredicate
        , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1))
      ]

      , LabelCheck "Create A"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass
      , Combine [
        validateLayersOrderPredicate
        , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      ]
    ]

everything_keyboard_test :: Test
everything_keyboard_test = constructTest "keyboard" pfstate_basic1 bs expected where
  bs = [
      EWCLabel "Copy pasta nothing"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl])

      , EWCLabel "Create A with random keyboard inputs in between"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'v') [])
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char '\\') [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
      , EWCNothing -- dummy to check state

      , EWCLabel "Copy pasta A"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl])

      , EWCLabel "Delete A using Delete key"
      , EWCKeyboard (KeyboardData (KeyboardKey_Delete) [])



      , EWCLabel "Select everything"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 (-100) (-100)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 200 200) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 200 200) True MouseButton_Left [] False)

      , EWCLabel "Cut pasta everything"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'x') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl])
    ]

  -- I can't remember why we're using checkLayerEntriesNum instead of counting number of entries in PFState but it doesn't matter, should be the same
  expected = [
      LabelCheck "Copy pasta nothing"
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1))
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1))

      , LabelCheck "Create A with random keyboard inputs in between"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1))
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1))
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      , Combine [
        validateLayersOrderPredicate
        , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
        , numSelectedEltsEqualPredicate 1
      ]

      , LabelCheck "Copy pasta A"
      , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 1)
      , Combine [
          -- TODO copy not implemented yet so this doesn't work
          checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 2)
          , numSelectedEltsEqualPredicate 1
        ]
      , Combine [
          checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 3)
          , numSelectedEltsEqualPredicate 1
        ]

      , LabelCheck "Delete A using Delete key"
      , Combine [
          checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 2)
          , numSelectedEltsEqualPredicate 0
        ]

      , LabelCheck "Select everything"
      , (EqPredicate _goatState_selectedTool Tool_Select)
      , checkHandlerNameAndState handlerName_select True
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate (length (_pFState_layers pfstate_basic1) + 2)

      , LabelCheck "Cut pasta everything"
      , checkLayerEntriesNum 0
      , Combine [
          numSelectedEltsEqualPredicate (length (_pFState_layers pfstate_basic1) + 2)
          , checkLayerEntriesNum (length (_pFState_layers pfstate_basic1) + 2)
        ]
    ]


everything_basic_test :: Test
everything_basic_test = constructTest "basic" emptyPFState bs expected where
  bs = [
      -- test basic panning
      EWCLabel "Pan"
      , EWCTool Tool_Pan
      -- drag to (1, 1) and release
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] False)
      -- drag to (10, 15) and cancel without releasing
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 9 14) False MouseButton_Left [] False)
      -- cancel and keep tracking and make sure nothing changes
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 9 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 (-20) 31) True MouseButton_Left [] False)

      -- create elt A
      , EWCLabel "Create A"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
      , EWCNothing -- dummy to check state

      , EWCLabel "create another elt, but cancel it"
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "press escape a bunch of times and make sure nothing breaks"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "create elt B"
      , EWCMouse (LMouseData (V2 0 20) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 20 30) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
      , EWCNothing -- dummy to check state

      -- unselect
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [] False)

      , EWCLabel "single click select elt B"
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [] False)

      -- now select elts A + B
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [] False)

      , EWCLabel "begin selecting nothing and cancel"
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 200 200) False MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 200 200) True MouseButton_Left [] False)

      , EWCLabel "single click shift unselect elt B"
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [KeyModifier_Shift] False)
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [KeyModifier_Shift] False)

      , EWCLabel "unselect"
      , EWCMouse (LMouseData (V2 100 100) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 100 100) True MouseButton_Left [] False)

      , EWCLabel "single click select elt A"
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] False)

      , EWCLabel "manipulate A"
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 (-2) (-2)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 (-2) (-2)) True MouseButton_Left [] False)

      , EWCLabel "single click shift select elt B"
      , EWCMouse (LMouseData (V2 1 21) False MouseButton_Left [KeyModifier_Shift] False)
      , EWCMouse (LMouseData (V2 1 21) True MouseButton_Left [KeyModifier_Shift] False)

      , EWCLabel "manipulate A+B"
      , EWCMouse (LMouseData (V2 5 5) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 7 5) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 7 5) True MouseButton_Left [] False)

      , EWCLabel "Mainpulate A+B then cancel"
      , EWCMouse (LMouseData (V2 7 5) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 7 5) True MouseButton_Left [] False)


      -- TODO delete the elt
      -- check in layers and check render
    ]

  expected = [
      LabelCheck "Pan"
      , (EqPredicate _goatState_selectedTool Tool_Pan)
      , (EqPredicate _goatState_pan (V2 0 0))
      , (EqPredicate _goatState_pan (V2 1 1))
      , (EqPredicate _goatState_pan (V2 1 1))
      , (EqPredicate _goatState_pan (V2 0 0))
      , (EqPredicate _goatState_pan (V2 10 15))
      , (EqPredicate _goatState_pan (V2 1 1))
      , AlwaysPass
      , (EqPredicate _goatState_pan (V2 1 1))

      , LabelCheck "Create A"
      , (EqPredicate _goatState_selectedTool Tool_Box)
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
      , checkHandlerNameAndState handlerName_empty False
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
      , (EqPredicate _goatState_selectedTool Tool_Select)
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
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == (-2) && y == (-2)
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
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 0 && y == (-2)
        _                                  -> False)

      , LabelCheck "Mainpulate A+B then cancel"
      , checkHandlerNameAndState handlerName_box True
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 3 && y == 3
        _                                  -> False)
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 0 && y == (-2)
        _                                  -> False)
      , firstSelectedSuperSEltLabelPredicate Nothing (\(_,_,SEltLabel _ selt) -> case selt of
        SEltBox (SBox (LBox (V2 x y) _) _) -> x == 0 && y == (-2)
        _                                  -> False)


    ]

spec :: Spec
spec = do
  describe "EverythingWidget" $ do
    fromHUnitTest $ everything_load_test
    fromHUnitTest $ everything_layers_test
    fromHUnitTest $ everything_keyboard_test
    fromHUnitTest $ everything_basic_test
