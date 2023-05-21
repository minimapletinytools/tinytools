module Potato.Flow.Controller.Manipulator.TestHelpers where

import           Relude

import           Potato.Flow.GoatTester
import           Potato.Flow            hiding (ex, ey)

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
      OwlItem _ (OwlSubItemLine _) -> Nothing
      x -> Just ("expected line, got " <> show x)
  verifySelectionIsAndOnlyIs "line is selected" f


verifyStateObjectHasProperty :: Text -> (OwlPFState -> Either Text a) -> (a -> Maybe Text) -> GoatTester ()
verifyStateObjectHasProperty name objectfetcher objectpredicate = verifyState name checkfn where
  checkfn gs = case objectfetcher (goatState_pFState gs) of
    Left e -> Just e
    Right o -> objectpredicate o


composeObjectFetcher :: (OwlPFState -> Either Text a) -> (a -> Either Text b) -> (OwlPFState -> Either Text b)
composeObjectFetcher f g pfs = case f pfs of
  Left e -> Left e
  Right o1 -> case g o1 of
    Left e -> Left e
    Right o2 -> Right o2


composeObjectFetcherKeep :: (OwlPFState -> Either Text a) -> (a -> Either Text b) -> (OwlPFState -> Either Text (a,b))
composeObjectFetcherKeep f g pfs = case f pfs of
  Left e -> Left e
  Right o1 -> case g o1 of
    Left e -> Left e
    Right o2 -> Right (o1, o2)
