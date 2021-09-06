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
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.Deprecated.Workspace

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq


-- first elt of second list is currently selected anchor (no anchor selected if empty)
-- by assumption each anchor can only differ in one component from the previous one 
-- anchors must not continue forward in same direction 
-- not ok: 1----2----3
--     ok: 1-3--2
data AnchorZipper = AnchorZipper [XY] [XY] deriving (Show)
emptyAnchorZipper :: AnchorZipper
emptyAnchorZipper = AnchorZipper [] []


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
        else (if dy == 0 then [] else [V2 (e1x+dx) (e1y + dy)]) <> (V2 e1x (e1y + dy) : ls)
      -- last was horizontal
      else if dy == 0
        -- if there was no vertical change, update the last point
        then pos:e2:es
        --last was horizontal, go vertical first
        else (if dx == 0 then [] else [V2 (e1x+dx) (e1y + dy)]) <> (V2 (e1x+dx) e1y : ls)
  

smartAutoPathDown :: XY -> [XY] -> [XY]
smartAutoPathDown pos es = reverse $ elbowFromEnd pos (reverse es)




instance PotatoHandler CartLineHandler where
  pHandlerName _ = handlerName_cartesianLine
  pHandleMouse clh@CartLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let

    -- restrict mouse
    dragDelta = _mouseDrag_to - _mouseDrag_from
    shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers
    mousexy = _mouseDrag_from + if shiftClick
      then restrict4 dragDelta
      else dragDelta

    in case _mouseDrag_state of
      MouseDragState_Down | _cartLineHandler_isCreation -> Just $ def {
        -- TODO
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler clh {
              _cartLineHandler_active = True
            }
        }
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
      MouseDragState_Down -> case _cartLineHandler_isCreation of

        True -> case _cartLineHandler_anchors of
          AnchorZipper _ (x:xs) -> error "this should never happen"
          -- creation case
          AnchorZipper [] [] -> r where
            -- TODO track the fact we clicked, if we drag, pass on to SimpleLine? (but what happens if we drag back to start)
            r = Just $ setHandlerOnly $ clh {
                _cartLineHandler_anchors = AnchorZipper [mousexy] []
              }
          AnchorZipper (x:xs) [] -> if last (x :| xs) == mousexy
            then Just $ setHandlerOnly $ clh {
                _cartLineHandler_isCreation = False
                , _cartLineHandler_active = False -- is it bad that we're still dragging but this is set to False?
              }
            else
              -- otherwise, smartly path dot to destination (always make 90 degree bend from current if possible)
              undefined
        False -> r where
          -- TODO
          -- if click on dot, store it
          -- if click on line, create new dot
          r = undefined
      MouseDragState_Dragging -> case _cartLineHandler_isCreation of
        -- TODO someday allow dragging dots on in creation case (to adjust position)
        True -> Just $ setHandlerOnly clh
        False -> r where
          -- TODO
          r = undefined
      MouseDragState_Up -> case _cartLineHandler_isCreation of
        -- nothing to do here
        True -> Just $ setHandlerOnly clh
        False -> r where
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
      }
    AnchorZipper fronts' backs' = _cartLineHandler_anchors
    fronts = fmap (toBoxHandle False) fronts'
    backs = case backs' of
      [] -> []
      x:xs -> toBoxHandle True x : fmap (toBoxHandle False) fronts'
    r = HandlerRenderOutput (fronts <> backs)
  pIsHandlerActive = _cartLineHandler_active
