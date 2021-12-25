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

import           Potato.Flow
import Potato.Flow.Render

-- test imports
import           Potato.Flow.Common
import           Potato.Flow.TestStates

import qualified Data.List.Ordered                 as L
import qualified Data.Sequence                     as Seq
import qualified Data.Vector.Unboxed     as V


hasUnsavedChanges  :: Bool -> EverythingPredicate
hasUnsavedChanges unsavedchanges = FunctionPredicate $
  (\gs -> ("expected " <> show unsavedchanges,
    goatState_hasUnsavedChanges gs == unsavedchanges))

expectState :: OwlPFState -> EverythingPredicate
expectState pfs = FunctionPredicate $
  (\pfs' -> ("expected owlpfstate_basic1",
    owlTree_equivalent (_owlPFState_owlTree pfs) (_owlPFState_owlTree pfs')
    && _owlPFState_canvas pfs == _owlPFState_canvas pfs'))
  . goatState_pFState

everything_load_test :: Test
everything_load_test = constructTest "load" emptyOwlPFState bs expected where
  bs = [
      EWCLabel "Load"
      , EWCLoad (owlPFState_to_sPotatoFlow owlpfstate_basic1, emptyControllerMeta)
      , EWCLoad (owlPFState_to_sPotatoFlow owlpfstate_basic1, emptyControllerMeta)
      , EWCLoad (owlPFState_to_sPotatoFlow owlpfstate_someValidState1, emptyControllerMeta)
      , EWCLoad (owlPFState_to_sPotatoFlow owlpfstate_basic1, emptyControllerMeta)
      , EWCLoad (owlPFState_to_sPotatoFlow owlpfstate_someValidState1, emptyControllerMeta)
      , EWCLoad (owlPFState_to_sPotatoFlow emptyOwlPFState, emptyControllerMeta)
    ]
  expected = [
      LabelCheck "Load"
      , expectState owlpfstate_basic1
      , expectState owlpfstate_basic1
      , expectState owlpfstate_someValidState1
      , Combine [
          expectState owlpfstate_basic1
          -- intersects "b1" "b2" "b3" "b4" in owlpfstate_basic1
          , numEltsInLBoxUsingBroadphasePredicate 4 (LBox 0 (V2 15 15))
        ]
      , expectState owlpfstate_someValidState1
      , Combine [
          expectState emptyOwlPFState
          , numEltsInLBoxUsingBroadphasePredicate 0 (LBox 0 (V2 1000 1000))
        ]


    ]

validateLayersOrderPredicate :: EverythingPredicate
validateLayersOrderPredicate = FunctionPredicate fn where
  fn GoatState {..} = r where
    lentries = _layersState_entries _goatState_layersState
    -- TODO report position of superowls in tree
    msg = "expected LayerEntries in order " <> show (fmap _layerEntry_superOwl lentries)
    owltree = _owlPFState_owlTree $ _owlPFWorkspace_pFState _goatState_workspace
    sortingfn le1 le2 = owlTree_superOwl_comparePosition owltree (_layerEntry_superOwl le1) (_layerEntry_superOwl le2) == LT
    r = (msg, L.isSortedBy sortingfn (toList lentries))

checkLayerEntriesNum :: Int -> EverythingPredicate
checkLayerEntriesNum n = r where
  r = FunctionPredicate $
    (\lentries -> ("expected " <> show n <> " got " <> show (Seq.length lentries), Seq.length lentries == n))
    . _layersState_entries . _goatState_layersState

-- TODO what does this even test???
everything_layers_test :: Test
everything_layers_test = constructTest "layers" owlpfstate_basic1 bs expected where
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
        , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)  )
      ]

      , LabelCheck "Create A"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass
      , Combine [
        validateLayersOrderPredicate
        , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      ]
    ]

-- test specific behaviour on input focus between layers and canvas
everything_inputfocusing_test :: Test
everything_inputfocusing_test = constructTest "inputfocusing" owlpfstate_basic1 bs expected where
  bs = [
      EWCLabel "Create A"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)
      , EWCNothing

      , EWCLabel "undo redo"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Esc) []) -- deselect so we don't go into renaming mode later

      -- TODO actually do something in layers
      , EWCLabel "click on layers"
      , EWCMouse (LMouseData (V2 10 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 10 0) True MouseButton_Left [] True)

      -- expect undo redo to work even though our focus is in layers now
      , EWCLabel "undo redo"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl])

      -- TODO (when it's implemented) add test for renaming in layers and then changing focus back to canvas should cancel rename action
    ]
  expected = [
      LabelCheck "Create A"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)

      , LabelCheck "undo redo"
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 0)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , AlwaysPass

      -- TODO test that something in layers got seleceted
      -- TODO test that last mouse input is in layers
      , LabelCheck "click on layers"
      , AlwaysPass
      , AlwaysPass

      , LabelCheck "undo redo"
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 0)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)

    ]

everything_canvasSize_test :: Test
everything_canvasSize_test = constructTest "canvas resize" owlpfstate_basic1 bs expected where
  bs = [
      EWCLabel "Initial"
      , EWCNothing -- dummy to check state

      , EWCLabel "Resize Canvas By"
      , EWCCanvasResize (V2 100 100)
    ]
  expected = [
      LabelCheck "Initial"
      -- TODO test canvas size (expect 50 25)
      , AlwaysPass

      , LabelCheck "Resize Canvas By"
      -- TODO test canvas size (expect 150 125)
      , AlwaysPass
    ]

everything_keyboard_test :: Test
everything_keyboard_test = constructTest "keyboard" owlpfstate_basic1 bs expected where
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

      , EWCLabel "Esc deselect everything"
      , EWCKeyboard (KeyboardData (KeyboardKey_Esc) [])
    ]

  -- I can't remember why we're using checkLayerEntriesNum instead of counting number of entries in PFState but it doesn't matter, should be the same
  expected = [
      LabelCheck "Copy pasta nothing"
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)  )
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)  )

      , LabelCheck "Create A with random keyboard inputs in between"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)  )
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)  )
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , Combine [
        validateLayersOrderPredicate
        , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
        , numSelectedEltsEqualPredicate 1
      ]

      , LabelCheck "Copy pasta A"
      , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 1)
      , Combine [
          -- TODO copy not implemented yet so this doesn't work
          checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 2)
          , numSelectedEltsEqualPredicate 1
        ]
      , Combine [
          checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 3)
          , numSelectedEltsEqualPredicate 1
        ]

      , LabelCheck "Delete A using Delete key"
      , Combine [
          checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 2)
          , numSelectedEltsEqualPredicate 0
        ]

      , LabelCheck "Select everything"
      , (EqPredicate _goatState_selectedTool Tool_Select)
      , checkHandlerNameAndState handlerName_select True
      , checkHandlerNameAndState handlerName_select True
      , numSelectedEltsEqualPredicate ((owlPFState_numElts owlpfstate_basic1)   + 2)

      , LabelCheck "Cut pasta everything"
      , checkLayerEntriesNum 0
      , Combine [
          numSelectedEltsEqualPredicate ((owlPFState_numElts owlpfstate_basic1)   + 2)
          , checkLayerEntriesNum ((owlPFState_numElts owlpfstate_basic1)   + 2)
        ]

      , LabelCheck "Esc deselect everything"
      , numSelectedEltsEqualPredicate 0

    ]

everything_newfolder_test :: Test
everything_newfolder_test = constructTest "new folder" owlpfstate_basic1 bs expected where
  bs = [
      EWCLabel "New Folder (no selection)"
      , EWCNewFolder

      , EWCLabel "New Folder (with selection)"
      -- TODO open up some folder, and select some stuff
      --, EWCNewFolder
    ]
  expected = [
      LabelCheck "New Folder (no selection)"
      , firstSelectedSuperOwlPredicate (Just "<folder>") (\sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltFolderStart -> True
        _                                        -> False)

      , LabelCheck "New Folder (with selection)"
    ]

moveOffset :: Int
moveOffset = 100

everything_lockhiddenselectionvialayers_test :: Test
everything_lockhiddenselectionvialayers_test = constructTestWithControllerMeta "lock hidden selection via folders" owlpfstate_basic1 controllermeta_basic1_lockandhidestuff1 bs expected where
  bs = [
      EWCLabel "Select b1" -- locked
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "Select b2"
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) True MouseButton_Left [] True)

      , EWCLabel "Select b3" -- hidden
      , EWCMouse (LMouseData (V2 moveOffset 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 2) True MouseButton_Left [] True)
    ]
  expected = [
      -- okay, turns out you can totally select locked/hidden stuff via folders lol
      -- TODO you might want to consider disallowing selection of locked stuff (directly NOT ok, via parents OK)
      LabelCheck "Select b1"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "Select b2"
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "Select b3"
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 1

    ]

everything_lockhiddenselectionviacanvas_test :: Test
everything_lockhiddenselectionviacanvas_test = constructTestWithControllerMeta "lock hidden selection via canvas" owlpfstate_basic1 controllermeta_basic1_lockandhidestuff1 bs expected where
  bs = [
      EWCLabel "Select b1" -- locked
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 0) True MouseButton_Left [] False)

      , EWCLabel "Select b2"
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "Select b3" -- hidden
      , EWCMouse (LMouseData (V2 0 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 0 10) True MouseButton_Left [] False)
    ]
  expected = [
      -- can't select b1 because locked
      LabelCheck "Select b1"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 0

      -- select b2 OK
      , LabelCheck "Select b2"
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 1

      -- can't select b3 because hidden
      , LabelCheck "Select b3"
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 0

    ]



everything_basic_test :: Test
everything_basic_test = constructTest "basic" emptyOwlPFState bs expected where
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
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 (-1) (-1)) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "press escape a bunch of times and make sure nothing breaks"
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])

      , EWCLabel "select elt A"
      , EWCTool Tool_Select
      , EWCMouse (LMouseData (V2 9 9) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 9 9) True MouseButton_Left [] False)

      , EWCLabel "create elt B"
      , EWCTool Tool_Text
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
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , Combine [
          PFStateFunctionPredicate (checkNumElts 1) -- make sure no elt was created
          , numSelectedEltsEqualPredicate 0 -- the newly created elt gets selected and after cancelling, the previous selection is lost, womp womp
          , checkHandlerNameAndState handlerName_empty False -- handler defaults to empty selection after cancelling :(
        ]
      -- same as above
      , Combine [
          PFStateFunctionPredicate (checkNumElts 1)
          , numSelectedEltsEqualPredicate 0
          , checkHandlerNameAndState handlerName_empty False
        ]

      , LabelCheck "press escape a bunch of times and make sure nothing breaks"
      , Combine [
          numSelectedEltsEqualPredicate 0
          , checkHandlerNameAndState handlerName_empty False
        ]
      , AlwaysPass
      , AlwaysPass
      , AlwaysPass

      , LabelCheck "select elt A"
      , EqPredicate _goatState_selectedTool Tool_Select
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "create elt B"
      , EqPredicate _goatState_selectedTool Tool_Text
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_boxText False
      , Combine [
          PFStateFunctionPredicate (checkNumElts 2) -- make sure second box was created
          , numSelectedEltsEqualPredicate 1
          , checkHandlerNameAndState handlerName_boxText False
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
      , firstSelectedSuperOwlPredicate Nothing (\sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == (-2) && y == (-2)
        _                                        -> False)

      , LabelCheck "single click shift select elt B"
      , checkHandlerNameAndState handlerName_select True -- single click shift unselect case uses select handler
      , numSelectedEltsEqualPredicate 2

      , LabelCheck "manipulate A+B"
      , checkHandlerNameAndState handlerName_box True
      , checkHandlerNameAndState handlerName_box True
      -- check that first elt A got moved over by 2
      -- TODO also check elt B
      , firstSelectedSuperOwlPredicate (Just "<box>") (\sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == 0 && y == (-2)
        _                                        -> False)

      , LabelCheck "Mainpulate A+B then cancel"
      , checkHandlerNameAndState handlerName_box True
      , firstSelectedSuperOwlPredicate (Just "<box>") (\sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == 3 && y == 3
        _                                        -> False)
      , firstSelectedSuperOwlPredicate (Just "<box>") (\sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == 0 && y == (-2)
        _                                        -> False)
      , firstSelectedSuperOwlPredicate (Just "<box>") (\sowl -> case hasOwlElt_toSElt_hack sowl of
        SEltBox (SBox (LBox (V2 x y) _) _ _ _ _) -> x == 0 && y == (-2)
        _                                        -> False)


    ]

-- test specific behaviour on input focus between layers and canvas
everything_hasSavedChanges_test :: Test
everything_hasSavedChanges_test = constructTest "has saved changes" owlpfstate_basic1 bs expected where
  bs = [
      EWCLabel "Start"
      , EWCNothing

      , EWCLabel "Create A"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)

      , EWCLabel "undo redo"
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl])

      , EWCLabel "save undo redo undo do"
      , EWCMarkSaved
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl])
      , EWCKeyboard (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl])
      -- TODO

      , EWCLabel "Create B"
      , EWCTool Tool_Box
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) False MouseButton_Left [] False)
      , EWCMouse (LMouseData (V2 10 10) True MouseButton_Left [] False)


    ]
  expected = [
      LabelCheck "Start"
      , hasUnsavedChanges False

      , LabelCheck "Create A"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , AlwaysPass
      , AlwaysPass
      , hasUnsavedChanges True

      , LabelCheck "undo redo"
      , hasUnsavedChanges False
      , hasUnsavedChanges True

      , LabelCheck "save undo redo undo do"
      , hasUnsavedChanges False
      , hasUnsavedChanges True
      , hasUnsavedChanges False
      , hasUnsavedChanges True

      , LabelCheck "Create B"
      , (EqPredicate _goatState_selectedTool Tool_Box)
      , AlwaysPass
      , AlwaysPass
      , hasUnsavedChanges True

    ]

-- bad because this function assumes:
-- - element render fills the entire box
-- - nothing else is overlapping it
-- - element is canonical and non zero
badTestVisibility  :: Text -> Bool -> EverythingPredicate
badTestVisibility name cansee = FunctionPredicate f where
  f gs = r where
    testForChar :: RenderedCanvasRegion -> XY -> Bool
    testForChar rc pos = case toIndexSafe (_renderedCanvasRegion_box rc) pos of
      Nothing -> False
      Just i -> _renderedCanvasRegion_contents rc V.! i /= ' '

    pfs = goatState_pFState gs

    -- why doesn't this work??? Nothing is getting rendered :(((
    rc = _goatState_renderedCanvas gs

    --rc = potatoRenderPFState pfs (emptyRenderedCanvasRegion (LBox 0 100))

    lmm = _layersState_meta $ _goatState_layersState gs

    msowl = hasOwlTree_test_findFirstSuperOwlByName pfs name
    wasrendered = case msowl of
      Nothing -> False
      Just sowl -> case getSEltBox (hasOwlElt_toSElt_hack sowl) of
        Nothing -> False
        Just box -> wasrendered' where
          wasrendered' = testForChar rc (_lBox_tl box)
    r = (show (_renderedCanvasRegion_box rc) <> show lmm <> " expected " <> show cansee <> " got " <> show wasrendered <> "\n\n" <> renderedCanvasToText rc <> "\n\n", cansee == wasrendered)

everything_hideStuff_test :: Test
everything_hideStuff_test = constructTestWithControllerMeta "render hide via folders test" owlpfstate_basic2 controllermeta_basic2_expandEverything bs expected where
  bs = [
      EWCLabel "Nothing"
      , EWCScreenRegion 25 -- must set the screen so we can see stuff

      , EWCLabel "hide b1"
      , EWCMouse (LMouseData (V2 3 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 3 2) True MouseButton_Left [] True)
      , EWCNothing
      , EWCNothing

      , EWCLabel "hide fstart1"
      , EWCMouse (LMouseData (V2 1 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 1 0) True MouseButton_Left [] True)

      , EWCLabel "show fstart1"
      , EWCMouse (LMouseData (V2 1 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 1 0) True MouseButton_Left [] True)

    ]
  expected = [
      LabelCheck "Nothing"
      , badTestVisibility "b1" True -- test b1 is not hidden

      , LabelCheck "hide b1"
      , AlwaysPass
      , AlwaysPass
      , badTestVisibility "b1" False -- test b1 is hidden
      , badTestVisibility "b2" True -- test b2 is not hidden

      , LabelCheck "hide fstart1"
      , badTestVisibility "b1" False -- test b1 is hidden
      , badTestVisibility "b2" False -- test b2 is hidden

      , LabelCheck "show fstart1"
      , badTestVisibility "b1" False -- test b1 is hidden
      , badTestVisibility "b2" True -- test b2 is not hidden

    ]


spec :: Spec
spec = do
  describe "EverythingWidget" $ do
    fromHUnitTest $ everything_load_test
    fromHUnitTest $ everything_layers_test
    fromHUnitTest $ everything_canvasSize_test
    fromHUnitTest $ everything_keyboard_test
    fromHUnitTest $ everything_lockhiddenselectionvialayers_test
    fromHUnitTest $ everything_lockhiddenselectionviacanvas_test
    fromHUnitTest $ everything_basic_test
    fromHUnitTest $ everything_inputfocusing_test
    fromHUnitTest $ everything_hasSavedChanges_test
    fromHUnitTest $ everything_hideStuff_test
