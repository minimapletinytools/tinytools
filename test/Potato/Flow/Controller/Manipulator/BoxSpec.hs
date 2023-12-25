{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.BoxSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Controller.Manipulator.TestHelpers


constrainDeltaLBox_test :: Spec
constrainDeltaLBox_test = do
  it "constrains as expected" $ do
    constrainDeltaLBox 1 (DeltaLBox 0 (V2 (-10) (-10))) ((LBox 0 (V2 5 5))) `shouldBe` DeltaLBox 0 (V2 (-4) (-4))
    constrainDeltaLBox 1 (DeltaLBox 0 (V2 (-10) 5)) ((LBox 0 (V2 5 5))) `shouldBe` DeltaLBox 0 (V2 (-4) 5)
    constrainDeltaLBox 1 (DeltaLBox (V2 10 0) (V2 (-10) 0)) ((LBox 0 (V2 5 5))) `shouldBe` DeltaLBox (V2 4 0) (V2 (-4) 0)

fetchLatestBox :: OwlPFState -> Either Text SBox
fetchLatestBox pfs = do
  sowl <- case maybeGetMostRecentlyCreatedOwl' pfs of
    Nothing -> Left "failed, no 🦉s"
    Just x  -> Right x
  case _owlItem_subItem (_superOwl_elt sowl) of
    OwlSubItemBox x -> Right x
    x                -> Left $ "expected SBox got: " <> show x

verifyMostRecentlyCreatedBoxLabelHasSize :: (Int, Int) -> GoatTester ()
verifyMostRecentlyCreatedBoxLabelHasSize (x, y) = verifyStateObjectHasProperty "verifyMostRecentlyCreatedBoxLabelHasSize" fetchLatestBox checkfn where 
  checkfn sbox = r where
    LBox _ (V2 x' y') = _sBox_box sbox
    r = if x == x' && y == y'
      then Nothing
      else Just $ "got size " <> show (x', y') <> " expected " <> show (x, y)

verifyMostRecentlyCreatedBoxHasText :: Maybe Text -> GoatTester ()
verifyMostRecentlyCreatedBoxHasText mt = verifyStateObjectHasProperty "verifyMostRecentlyCreatedBoxHasText" fetchLatestBox checkfn where
  checkfn sbox = r where
    istext = sBoxType_isText (_sBox_boxType sbox)
    r = case 
      mt of 
        Nothing -> if istext then Just "expected no text" else Nothing
        Just t -> if not istext 
          then Just "expected text" 
          else if t == _sBoxText_text (_sBox_text sbox)
            then Nothing 
            else Just $ "got text " <> show (_sBox_text sbox) <> " expected " <> t




initSimpleBox :: GoatTester ()
initSimpleBox = drawCanvasBox (0, 0, 100, 100)

basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleBox

  -- TODO

basic_cancel_test :: Spec
basic_cancel_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "mouse down and cancel and ensure no box is created"
  setTool Tool_Box
  canvasMouseDown (0, 0)
  pressEscape
  canvasMouseDown (100, 100)
  canvasMouseUp (100, 100)
  verifyOwlCount 0

  setMarker "draw a line and cancel after moving it"
  setTool Tool_Box
  canvasMouseDown (0, 0)
  canvasMouseDown (100, 100)
  verifyOwlCount 1
  pressEscape
  verifyOwlCount 0
  canvasMouseDown (110, 90)
  canvasMouseUp (110, 90)
  verifyOwlCount 0

  initSimpleBox

noinvert_test :: Spec
noinvert_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  drawCanvasBox (0, 0, 101, 101)

  setMarker "resize the box"
  canvasMouseDown (101,101)
  canvasMouseDown (-10,-10)
  canvasMouseUp (-10,-10)
  verifyMostRecentlyCreatedBoxLabelHasSize (1, 1)

  
boxtext_test :: Spec
boxtext_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  drawCanvasBox (0, 0, 101, 101)

  verifyMostRecentlyCreatedBoxHasText Nothing

  setMarker "click in the box to convert it to a text box"
  canvasMouseDown (50,50)
  canvasMouseUp (50,50)
  verifyMostRecentlyCreatedBoxHasText (Just "")

  setMarker "write some text"
  pressKeys "meow meow meow meow"
  verifyMostRecentlyCreatedBoxHasText (Just "meow meow meow meow")


manipulator_basic_test :: Spec
manipulator_basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  drawCanvasBox (0, 0, 100, 100)

  handlers <- fmap _handlerRenderOutput_temp getHandlerRenderOutput

  verifyBool "has 5 handlers" (length handlers == 5)
  verifyBool "has handler at upper left corner" (any (\(RenderHandle (LBox (V2 x y) _) _ _) -> x == (-1) && y == (-1)) handlers)
  --verifyBool "has handler on label" (any (\(HandlerRenderOutput (LBox (V2 x y) _) _ _) -> x == 1 && y == 0) handlers)
  verifyBool "has handler on the box area" (any (\(RenderHandle (LBox (V2 x y) (V2 w h)) _ _) -> x == 1 && y == 1 && w == 98 && h == 98) handlers)



spec :: Spec
spec = do
  describe "Box" $ do
    describe "basic" $ basic_test
    describe "basic_cancel" $ basic_cancel_test
    describe "constrainDeltaLBox" $ constrainDeltaLBox_test
    describe "noinvert" $ noinvert_test
    describe "boxtext" $ boxtext_test
