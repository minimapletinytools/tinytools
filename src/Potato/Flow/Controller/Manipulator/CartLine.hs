-- TODO DELETE ME
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.CartLine (
  CartLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math

import           Control.Exception
import           Data.Default
import qualified Text.Pretty.Simple as Pretty
import qualified Data.Text.Lazy as LT


{- examples of how CartLine works

1---2
    |
    3
drag 2 up (1 moves as well)
1---2
    |
    |
    3
drag 2 right (3 moves as well)
1------2
       |
       |
       3

1---x---2
drag x down (later anchor always moves)
1---*
    |
    *---2

examples:
1---3---2
drag 2 up
    3---2
        |
1-------*

-}

isCartesian :: XY -> XY -> Bool
isCartesian (V2 ax ay) (V2 bx by) = ax == bx || ay == by

-- | predicate holds if pt is between a and b
-- expects a b to be in the same cartesian plane
isBetween :: XY -> (XY, XY) -> Bool
isBetween (V2 px py) (a@(V2 ax ay), b@(V2 bx by)) = assert (isCartesian a b) $ if ax == bx && ax == px
  -- if in same vertical line
  then (py >= ay && py <= by) || (py <= ay && py >= by)
  else if ay == by && ay == py
    -- if in same horizontal line
    then (px >= ax && px <= bx) || (px <= ax && px >= bx)
    else False

splitFind :: (a -> Bool) -> [a] -> ([a],[a])
splitFind p l = r where
  splitFind' rprevs [] = (rprevs,[])
  splitFind' rprevs (x:xs) = if p x
    -- note we built up backwards but we reverse at the very end
    then (reverse rprevs, x:xs)
    else splitFind' (x:rprevs) xs
  r = splitFind' [] l

-- first elt of second list is currently selected anchor (no anchor selected if empty)
-- by assumption each anchor can only differ in one component from the previous one
-- anchors must not continue forward in same direction
-- not ok: 1----2----3
--     ok: 1-3--2
data AnchorZipper = AnchorZipper [XY] [XY] deriving (Show)
emptyAnchorZipper :: AnchorZipper
emptyAnchorZipper = AnchorZipper [] []

flattenAnchors :: AnchorZipper -> [XY]
flattenAnchors (AnchorZipper xs ys) = xs <> ys

-- | flatten AnchorZipper to a plain list
-- used only in creation step, where no anchor can be focused, asserts if this condition fails
flattenAnchorsInCreation :: AnchorZipper -> [XY]
flattenAnchorsInCreation az@(AnchorZipper xs ys) = assert (length ys == 0) $ flattenAnchors az


-- | adjacentPairs [1,2,3,4] `shouldBe` [(1,2),(2,3),(3,4)]
adjacentPairs :: [a] -> [(a,a)]
adjacentPairs [] = []
adjacentPairs (x:[]) = []
adjacentPairs (x:y:es) = (x,y) : adjacentPairs (y:es)


-- TODO TEST
-- | validate if AnchorZipper assumptions hold
validateAnchorZipper :: AnchorZipper -> Bool
validateAnchorZipper (AnchorZipper xs1 xs2) = r where

  check1 (V2 ex ey) (V2 l1x l1y) = if ex == l1x
    then ey /= l1y
    else ey == l1y
  check2 (V2 ex ey) (V2 l1x l1y) (V2 l2x l2y) = if l1x == l2x
    -- last one was vertical, expect horizontal or reversal
    then ey == l1y || l1x - l2x > ex - l2x
    -- last one was horizontal, expect vertical or reversal
    else ex == l1x || l1y - l2y > ey - l2y

  foldfn e (pass, mlast1, mlast2) = if not pass
    then (False, Nothing, Nothing)
    else case mlast1 of
      Just last1 -> case mlast2 of
        Just last2 -> (check2 e last1 last2 , Just e, Just last1)
        Nothing -> (check1 e last1, Just e, Just last1)
      Nothing -> (True, Just e, Nothing)

  (r, _, _) = foldr foldfn (True, Nothing, Nothing) (xs1<>xs2)

data CartLineHandler = CartLineHandler {
    _cartLineHandler_anchors      :: AnchorZipper
    , _cartLineHandler_undoFirst  :: Bool
    , _cartLineHandler_isCreation :: Bool
    , _cartLineHandler_active     :: Bool
  } deriving (Show)

instance Default CartLineHandler where
  def = CartLineHandler {
      _cartLineHandler_anchors = emptyAnchorZipper
      , _cartLineHandler_undoFirst = False
      , _cartLineHandler_isCreation = False
      , _cartLineHandler_active = False
    }


-- | get the last 2 elements of e1:e2:es
-- DELETE
last2 :: XY -> XY -> [XY] -> (XY, XY)
last2 e1 e2 es = r where
  l1 = last (e1:|e2:es)
  l2 = case (reverse es) of
    [] -> e1
    x:xs -> case xs of
      [] -> e2
      _ -> x
  r = (l1, l2)

-- helper method for creating new anchor at the end of a sequence of anchors (when creating new lines)
-- both input and output anchor list is REVERSED
elbowFromEnd :: XY -> [XY] -> [XY]
elbowFromEnd pos [] = [pos]
elbowFromEnd pos (e:[]) = r where
  V2 e1x e1y = e
  V2 dx dy = pos - e
  r = reverse $ if dx > dy
    then [e, V2 (e1x+dx) e1y] <> if dy == 0 then [] else [V2 (e1x+dx) (e1y + dy)]
    else [e, V2 e1x (e1y + dy)] <> if dx == 0 then [] else [V2 (e1x+dx) (e1y + dy)]
elbowFromEnd pos ls@(e1:(e2:es)) = r where
  V2 e1x e1y = e1
  V2 e2x e2y = e2
  V2 dx dy = pos - e1
  r = if dx == 0 && dy == 0
    then ls
    else if e1x == e2x
      -- if last was vertical
      then if dx == 0
        -- if there was no horizontal change, update the last point
        then pos:e2:es
        --last was vertical, go horizontal first
        else (if dy == 0 then [] else [V2 (e1x+dx) (e1y + dy)]) <> (V2 (e1x+dx) e1y : ls)
      -- last was horizontal
      else if dy == 0
        -- if there was no vertical change, update the last point
        then pos:e2:es
        --last was horizontal, go vertical first
        else (if dx == 0 then [] else [V2 (e1x+dx) (e1y + dy)]) <> (V2 e1x (e1y + dy) : ls)


smartAutoPathDown :: XY -> [XY] -> [XY]
smartAutoPathDown pos es = reverse $ elbowFromEnd pos (reverse es)




instance PotatoHandler CartLineHandler where
  pHandlerName _ = handlerName_cartesianLine
  pHandlerDebugShow clh = LT.toStrict $ Pretty.pShowNoColor clh
  pHandleMouse clh@CartLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let

    -- restrict mouse
    dragDelta = _mouseDrag_to - _mouseDrag_from
    shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers
    mousexy = _mouseDrag_from + if shiftClick
      then restrict4 dragDelta
      else dragDelta

    anchors = flattenAnchors _cartLineHandler_anchors

    in case _mouseDrag_state of
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing

      -- TODO creation should be a separate handler
      -- creation case
      MouseDragState_Down | _cartLineHandler_isCreation -> case _cartLineHandler_anchors of
        AnchorZipper _ (x:xs) -> error "this should never happen"
        AnchorZipper [] [] -> r where
          -- TODO track the fact we clicked, if we drag, pass on to SimpleLine? (but what happens if we drag back to start??)
          r = Just $ setHandlerOnly $ clh {
              _cartLineHandler_active = True
              , _cartLineHandler_anchors = AnchorZipper [mousexy] []
            }
        AnchorZipper (x:xs) [] -> if last (x :| xs) == mousexy
          -- if we click on the last dot, we're done, exit creation mode
          then Just $ setHandlerOnly $ clh {
              _cartLineHandler_isCreation = True
              , _cartLineHandler_active = False -- is it bad that we're still dragging but this is set to False?
            }
          -- otherwise, smartly path dot to destination (always make 90 degree bend from current if possible)
          else Just $ setHandlerOnly $ clh {
              _cartLineHandler_anchors = AnchorZipper
                (smartAutoPathDown mousexy (flattenAnchorsInCreation _cartLineHandler_anchors))
                []
            }
      -- TODO someday allow dragging dots on in creation case (to adjust position)
      MouseDragState_Dragging | _cartLineHandler_isCreation -> Just $ setHandlerOnly clh
      MouseDragState_Up | _cartLineHandler_isCreation ->  Just $ setHandlerOnly clh {
          -- disable creation mode on release (no reason besides it being convenient code wise)
          _cartLineHandler_isCreation = _cartLineHandler_active
        }

      -- modify existing line case
      MouseDragState_Down -> r where
        -- first go through and find dots we may have clicked on
        (dotfs,dotbs) = splitFind (== mousexy) anchors
        -- then go through and find any lines we may have clicked on
        (linefs, linebs) = splitFind (isBetween mousexy) (adjacentPairs anchors)

        r = if null dotbs
          -- we did not click on any dots
          then if null linebs
            -- we found nothing, input not captured
            then Nothing
            -- we clicked on a line
            else undefined -- TODO
          -- we clicked on a dot
          else undefined -- TODO

      -- TODO
      MouseDragState_Dragging -> r where
        r = undefined
      MouseDragState_Up -> r where
        -- on release cases, topology may change (some anchors removed), unclear how to map topology (probably need meta data to track)
        -- if release is on a dummy dot, (in between two other dots)
        -- TODO
        r = undefined

      MouseDragState_Cancelled -> Just def

  pHandleKeyboard clh PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement based on last selected manipulator I guess
    _                              -> Nothing

  pRenderHandler clh@CartLineHandler {..} PotatoHandlerInput {..} = r where
    toBoxHandle isactive xy = RenderHandle {
        _renderHandle_box = LBox xy 1
        , _renderHandle_char = if isactive then Just '+' else Just 'X'
        , _renderHandle_color = RHC_Default
      }
    AnchorZipper fronts' backs' = _cartLineHandler_anchors
    fronts = fmap (toBoxHandle False) fronts'
    backs = case backs' of
      [] -> []
      x:xs -> toBoxHandle True x : fmap (toBoxHandle False) fronts'
    r = HandlerRenderOutput (fronts <> backs)
  pIsHandlerActive = _cartLineHandler_active

  pHandlerTool CartLineHandler {..} = if _cartLineHandler_isCreation
    then Just Tool_CartLine
    else Nothing
