{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.GoatCmdSpec (
  spec
) where

import           Relude              

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow

verifyCanvasSize :: (Int, Int) -> GoatTester ()
verifyCanvasSize (w,h) = verifyState "verifyCanvasSize" f where
  f gs = r where
    SCanvas (LBox _ (V2 w' h')) = _owlPFState_canvas $ goatState_pFState gs
    r = if w == w' && h == h'
      then Nothing
      else Just $ "expected: " <> show (w, h) <> " got " <> show (w', h')

deltaResizeCanvasCmd :: (Int, Int) -> GoatCmd
deltaResizeCanvasCmd (w, h) = GoatCmdWSEvent $ WSEResizeCanvas (DeltaLBox 0 (V2 w h))

goatCmdWSEvent_basic_test ::  Spec
goatCmdWSEvent_basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  verifyCanvasSize (1,1)

  setMarker "resize canvas"
  runCommand $ deltaResizeCanvasCmd (10, 0)
  verifyCanvasSize (11,1)
  runCommand $ deltaResizeCanvasCmd (-5, 5)
  verifyCanvasSize (6,6)



goatCmdWSEvent_invalid_test ::  Spec
goatCmdWSEvent_invalid_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  verifyCanvasSize (1,1)

  setMarker "resize canvas"
  runCommand $ deltaResizeCanvasCmd (-1, 0)
  verifyCanvasSize (1,1)
  

spec :: Spec
spec = do
  describe "GoatCmd" $ do
    describe "goatCmdWSEvent_basic_test" $ goatCmdWSEvent_basic_test
    describe "goatCmdWSEvent_invalid_test" $ goatCmdWSEvent_invalid_test
