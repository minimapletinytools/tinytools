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


data AnchorZipper = AnchorZipper [XY] [XY] deriving (Show)
emptyAnchorZipper :: AnchorZipper
emptyAnchorZipper = AnchorZipper [] []

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


-- TODO some helper function for updating _cartLineHandler_anchors from selection
-- TODO note on drag cases, topology should always match
-- on release cases, topology may change (some anchors removed), unclear how to map topology (probably need meta data to track)

instance PotatoHandler CartLineHandler where
  pHandlerName _ = handlerName_cartesianLine
  pHandleMouse clh@CartLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of

    MouseDragState_Down | _cartLineHandler_isCreation -> Just $ def {
      -- TODO
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler clh {
            _cartLineHandler_active = True
          }
      }
    -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
    -- TODO consider moving this into GoatWidget since it's needed by many manipulators
    MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
    MouseDragState_Down -> undefined
    MouseDragState_Dragging -> undefined
    MouseDragState_Up -> undefined
    MouseDragState_Cancelled -> Just def

  pHandleKeyboard clh PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement based on last selected manipulator I guess
    _                              -> Nothing
  pRenderHandler clh@CartLineHandler {..} PotatoHandlerInput {..} = r where
    -- TODO
    boxes = []
    r = HandlerRenderOutput boxes
  pIsHandlerActive = _cartLineHandler_active
