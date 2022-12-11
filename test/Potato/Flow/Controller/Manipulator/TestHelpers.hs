module Potato.Flow.Controller.Manipulator.TestHelpers where

import           Relude

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Common

import qualified Data.List              as L

drawCanvasBox :: (Int, Int, Int, Int) -> GoatTester ()
drawCanvasBox (x, y, sx, sy) = do
  count <- getOwlCount
  setTool Tool_Box
  canvasMouseDown (x, y)
  canvasMouseDown (x+sx, y+sy)
  verifyOwlCount (count+1)
  canvasMouseUp (x+sx, y+sy)
  let
    f sowl = case _superOwl_elt sowl of
      OwlItem _ (OwlSubItemBox _) -> Nothing
      x                           -> Just ("expected box, got " <> show x)
  verifySelectionIsAndOnlyIs "box is selected" f







drawCanvasLine :: (Int, Int) -> (Int, Int) -> GoatTester ()
drawCanvasLine (sx, sy) (ex, ey) = do
  count <- getOwlCount
  setTool Tool_Line
  canvasMouseDown (sx, sy)
  canvasMouseDown (ex, ey)
  verifyOwlCount (count+1)
  canvasMouseUp (ex, ey)
  let
    f sowl = case _superOwl_elt sowl of
      OwlItem _ (OwlSubItemLine _ _) -> Nothing
      x -> Just ("expected line, got " <> show x)
  verifySelectionIsAndOnlyIs "line is selected" f
