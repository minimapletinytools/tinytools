{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.TextAreaSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow
import           Potato.Flow.GoatTester
import           Potato.Flow.Controller.Manipulator.TestHelpers


import qualified Data.Sequence                                  as Seq





basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState (emptyOwlStateWithSize (100,100)) $ do

  setMarker "create a TextArea"
  setTool Tool_TextArea
  canvasMouseDown (0, 0)
  canvasMouseDownUp (50, 50)
  verifyOwlCount 1

  setMarker "add some text"
  pressKeys "poop"
  verifyRenderNonEmptyCount 4
  verifyCharRenderedAt (V2 0 0) (Just 'p')


  setMarker "deselect then reselect"
  verifyCanvasHandler handlerName_textArea
  pressEscape 
  verifyCanvasHandler handlerName_box
  pressEscape
  canvasMouseDownUp (25, 25)

  setMarker "move cursor"
  canvasMouseDownUp (0, 1)
  pressKeys "meow"
  verifyRenderNonEmptyCount 8
  verifyCharRenderedAt (V2 3 1) (Just 'w')




spec :: Spec
spec = do
  describe "TextArea" $ do
    describe "basic_test" $ basic_test
    
