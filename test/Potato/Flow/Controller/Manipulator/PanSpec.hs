module Potato.Flow.Controller.Manipulator.PanSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Controller.Manipulator.TestHelpers



-- | verifies that the number of owls (elts) in the state is what is expected, includes folders in the count
verifyPan :: (Int, Int) -> GoatTester ()
verifyPan expected = verifyState "verifyPan" f where
  f gs = if pan == expected
    then Nothing
    else Just $ "got " <> show pan <> " expected " <> show expected
    where
      V2 x y = _goatState_pan gs
      pan = (x,y)


verifyScreenSize :: V2 Int -> GoatTester ()
verifyScreenSize expected = verifyState "verifyScreenSize" f where
  f gs = if screensize == expected
    then Nothing
    else Just $ "got " <> show screensize <> " expected " <> show expected
    where
      screensize = _goatState_screenRegion gs



-- TODO broken...
basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  drawCanvasBox (0, 0, 10, 10)
  -- TODO verify box position

  let 
    pan1 = (1, 1)
    pan2 = (5, -7)
    -- after panning to pan1 when we pan to pan2 it's actually panning to pan2+pan1
    pan2p1 = (6,-6)
  setMarker "pan"
  verifyPan (0,0)
  setTool Tool_Pan
  canvasMouseDown (0, 0)
  canvasMouseDown pan1
  verifyPan pan1
  canvasMouseDown pan2
  verifyPan pan2p1
  -- now we are at 
  canvasMouseUp (0, 0)
  verifyPan pan2p1

  setMarker "draw a box"
  drawCanvasBox (0, 0, 10, 10)
  -- TODO verify box position is offset by pan


cancel_test :: Spec
cancel_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  let pan = (5, -7)
  setMarker "pan and cancel"
  verifyPan (0,0)
  setTool Tool_Pan
  canvasMouseDown (0, 0)
  canvasMouseDown pan
  verifyPan pan
  pressEscape
  canvasMouseDown pan
  verifyPan (0, 0)


middle_button_pan_test :: Spec
middle_button_pan_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "set tool to box, then pan using middle mouse button"
  setTool Tool_Box
  mouse True False [] MouseButton_Middle (V2 0 0)
  mouse True False [] MouseButton_Middle (V2 10 10)
  mouse True True [] MouseButton_Middle (V2 10 10)

  setMarker "check we went back to box handler"
  verifyOwlCount 0
  canvasMouseDown (0, 0)
  canvasMouseDown (1, 1)
  canvasMouseUp (1, 1)
  verifyOwlCount 1


pan_resize_pan_test :: Spec
pan_resize_pan_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  let
    pan = (5, 10)
    minuspan = (-5, -10)
    newscreensize = V2 150 150

  setMarker "draw a box"
  drawCanvasBox (0, 0, 10, 10)
  
  setMarker "pan"
  verifyPan (0,0)
  setTool Tool_Pan
  canvasMouseDown (0, 0)
  canvasMouseUp pan
  verifyPan pan

  setMarker "resize"
  verifyScreenSize goatTesterInitialScreenSize
  resizeScreen newscreensize
  verifyScreenSize newscreensize
  
  setMarker "pan back"
  setTool Tool_Pan
  canvasMouseDown (0, 0)
  canvasMouseUp minuspan
  verifyPan (0, 0)
  -- TODO verify box position on screen

  setMarker "resize back"
  resizeScreen goatTesterInitialScreenSize
  verifyScreenSize goatTesterInitialScreenSize
  -- TODO verify something

spec :: Spec
spec = do
  describe "Pan" $ do
    describe "basic_test" $ basic_test
    describe "cancel_test" $ cancel_test
    describe "middle_button_pan_test" $ middle_button_pan_test
    describe "pan_resize_pan_test" $ pan_resize_pan_test
