{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Layers (
  LayersHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Owl
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.Types
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.OwlState

import           Data.Default
import qualified Data.IntMap                    as IM
import qualified Data.Sequence                  as Seq
import Data.Sequence ((<|))
import           Data.Tuple.Extra

data LayerDragState = LDS_None | LDS_Dragging | LDS_Selecting LayerEntryPos deriving (Show, Eq)

data LayerDownType = LDT_Hide | LDT_Lock | LDT_Collapse | LDT_Normal deriving (Show, Eq)


-- TODO we could probably change this to do a more efficient binary search based on position in hierarchy
doesSelectionContainREltId :: REltId -> Selection -> Bool
doesSelectionContainREltId rid = isJust . find (\sowl -> rid == _superOwl_id sowl) . unSuperOwlParliament

-- TODO a little weird to be returning the SuperOwl you clicked on but whatever...
clickLayerNew :: Seq LayerEntry -> XY -> Maybe (SuperOwl, LayerDownType, Int)
clickLayerNew lentries  (V2 absx lepos) = case Seq.lookup lepos lentries of
  Nothing                      -> Nothing
  Just le -> Just . (,,absx - layerEntry_depth le) sowl $ case () of
    () | layerEntry_isFolder le && layerEntry_depth le == absx -> LDT_Collapse
    () | layerEntry_depth le + 1 == absx   -> LDT_Hide
    () | layerEntry_depth le + 2 == absx -> LDT_Lock
    () -> LDT_Normal
    where
      sowl = _layerEntry_superOwl le


data LayersHandler = LayersHandler {
    _layersHandler_dragState   :: LayerDragState
    , _layersHandler_cursorPos :: XY
    , _layersHandler_dropSpot :: Maybe OwlSpot

  }

instance Default LayersHandler where
  def = LayersHandler {
      _layersHandler_dragState = LDS_None
      , _layersHandler_cursorPos = 0
    }

-- TODO
make_layersHandlerRenderOutput :: PotatoHandlerInput -> LayersHandler -> LayersViewHandlerRenderOutput
make_layersHandlerRenderOutput PotatoHandlerInput {..} LayersHandler {..} = LayersViewHandlerRenderOutput newlentries where
  selection = _potatoHandlerInput_selection
  ls@(LayersState lmm lentries scrollPos) = _potatoHandlerInput_layersState
  --pfs = _potatoHandlerInput_pFState
  --owltree = (_owlPFState_owlTree pfs)

  isSelected lentry = doesSelectionContainREltId (_superOwl_id $ _layerEntry_superOwl lentry) selection

  newlentries' = fmap (\lentry -> LayersHandlerRenderEntryNormal (isSelected lentry) lentry) lentries

  newlentries = case _layersHandler_dropSpot of
    Nothing -> newlentries'
    Just ds -> r where
      mleftmost = case _owlSpot_parent ds of
          x | x == noOwl -> maybe Nothing Just (_owlSpot_leftSibling ds)
          x -> case _owlSpot_leftSibling ds of
            Nothing -> Just x
            Just s -> Just s


      r = case mleftmost of
        Nothing -> LayersHandlerRenderEntryDummy 0 <| newlentries'
        Just sibid -> r' where
          (index, depth) = case Seq.findIndexL (\lentry -> _superOwl_id (_layerEntry_superOwl lentry) == sibid) lentries of
            Nothing -> error $ "expected to find id " <> show sibid <> " in " <> show lentries
            Just x -> (x, layerEntry_depth (Seq.index lentries x))
          -- TODO correct depth calculation
          r' = Seq.insertAt index (LayersHandlerRenderEntryDummy depth) newlentries'


instance PotatoHandler LayersHandler where
  pHandlerName _ = handlerName_simpleLine

  -- we incorrectly reuse RelMouseDrag for LayersHandler even though LayersHandler doesn't care about canvas pan coords
  -- pan offset should always be set to 0 in RelMouseDrag
  pHandleMouse lh@LayersHandler {..} PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = let
    selection = _potatoHandlerInput_selection
    ls@(LayersState lmm lentries scrollPos) = _potatoHandlerInput_layersState
    pfs = _potatoHandlerInput_pFState
    owltree = (_owlPFState_owlTree pfs)

    rawleposxy@(V2 rawxoffset rawlepos) = _mouseDrag_to
    leposxy@(V2 _ lepos) = V2 rawxoffset (rawlepos + scrollPos)

    in case (_mouseDrag_state, _layersHandler_dragState) of
      (MouseDragState_Down, LDS_None) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        (nextDragState, mNextLayerState, changes) = case clickLayerNew lentries leposxy of
          Nothing -> (LDS_None, Nothing, IM.empty)
          -- (you can only click + drag selected elements)
          Just (downsowl, ldtdown, offset) -> case ldtdown of
            LDT_Normal -> if shift || (not $ doesSelectionContainREltId (_superOwl_id downsowl) selection)
              -- if element wasn't selected or shift is held down, enter selection mode
              then (LDS_Selecting lepos, Nothing, IM.empty)
              else (LDS_Dragging, Nothing, IM.empty)
            LDT_Hide -> (LDS_None, Just $ toggleLayerEntry pfs ls lepos LHCO_ToggleHide, IM.empty)
            LDT_Lock -> r' where
              nextLayersState = toggleLayerEntry pfs ls lepos LHCO_ToggleLock
              hideChanges = changesFromToggleHide pfs nextLayersState lepos
              r' = (LDS_None, Just $ nextLayersState, hideChanges)
            LDT_Collapse -> (LDS_None, Just $ toggleLayerEntry pfs ls lepos LHCO_ToggleCollapse, IM.empty)

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = nextDragState
                , _layersHandler_cursorPos = _mouseDrag_to
                , _layersHandler_dropSpot = Nothing
              }
            , _potatoHandlerOutput_layersState = mNextLayerState
            , _potatoHandlerOutput_changesFromToggleHide = changes
          }
      (MouseDragState_Down, _) -> error "unexpected, _layersHandler_dragState should have been reset on last mouse up"
      (MouseDragState_Dragging, LDS_Dragging) -> r where

        -- we will always place between dropSowl and justAboveDropSowl
        mDropSowlWithOffset = do
          (downsowl, _, offset') <- clickLayerNew lentries leposxy
          return (downsowl, offset')

        mJustAboveDropSowl = do
          lentry <- case mDropSowlWithOffset of
            Nothing -> Seq.lookup (Seq.length lentries - 1) lentries
            Just _ -> Seq.lookup (lepos-1) lentries
          return $ _layerEntry_superOwl lentry

        nparentoffset' = case mDropSowlWithOffset of
          Nothing -> case mJustAboveDropSowl of
            Nothing -> error "this should never happen"
            Just sowl -> rawxoffset - superOwl_depth sowl
          Just (_, x) -> x

        -- clamp max amount between the two locations
        -- TODO
        nparentoffset = case mDropSowlWithOffset of
          Nothing -> nparentoffset'
          Just (dsowl,_) -> case mJustAboveDropSowl of
            Nothing -> error "this should never happen"
            -- do not let negative parent offset go past difference
            Just asowl -> max nparentoffset' (superOwl_depth asowl - superOwl_depth dsowl)

        -- determine which direction we're moving the mouse in
        --V2 _ lastCursorY = _layersHandler_cursorPos
        --V2 _ cursorY = _mouseDrag_to
        --goingDown = lastCursorY > cursorY

        -- TODO do something with this
        targetspot = case mJustAboveDropSowl of
          -- we are dropping at the top of our LayerEntries
          Nothing -> OwlSpot noOwl Nothing
          Just sowl -> if nparentoffset > 0 && isOwl_isFolder sowl
            -- drop inside at the top
            then OwlSpot (_superOwl_id sowl) Nothing
            else case owlTree_findSuperOwl owltree newsiblingid of
              Nothing -> OwlSpot noOwl siblingout
              Just newsibling -> OwlSpot (superOwl_parentId newsibling) siblingout
              where
                newsiblingid = owlTree_superOwlNthParentId owltree sowl (-(min 0 nparentoffset))
                siblingout = case newsiblingid of
                  x | x == noOwl -> Nothing
                  x -> Just x

        r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_cursorPos = _mouseDrag_to
              , _layersHandler_dropSpot = Just targetspot
            }
        }

      -- TODO someday do drag for multi-select here
      (MouseDragState_Dragging, _) -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_cursorPos = _mouseDrag_to
              , _layersHandler_dropSpot = Nothing
            }
        }

      -- TODO if mouse didn't move from lposdown, enter renaming mode (new handler I guess)
      (MouseDragState_Up, LDS_Selecting leposdown) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        sowl = _layerEntry_superOwl $ Seq.index lentries leposdown
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = LDS_None
                , _layersHandler_dropSpot = Nothing
              }
            , _potatoHandlerOutput_select = Just (shift, SuperOwlParliament $ Seq.singleton sowl)
          }

      -- TODO when we have multi-user mode, we'll want to test if the target drop space is still valid
      (MouseDragState_Up, LDS_Dragging) -> r where
        mev = do
          spot <- _layersHandler_dropSpot
          let
            -- TODO spot is invalid if it's the child of a already select elt
            isSpotValid = True
            -- TODO modify if we drag on top of existing elt
            modifiedSpot = spot
          guard isSpotValid
          return $ WSEMoveElt (modifiedSpot, superOwlParliament_toOwlParliament selection)

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = LDS_None

              }
            , _potatoHandlerOutput_pFEvent = mev
          }



      (MouseDragState_Up, LDS_None) -> Just $ setHandlerOnly lh
      (MouseDragState_Cancelled, _) -> Just $ setHandlerOnly lh {
          _layersHandler_dragState = LDS_None
          , _layersHandler_dropSpot = Nothing
        }
      _ -> error $ "unexpected mouse state passed to handler " <> show _mouseDrag_state <> " " <> show _layersHandler_dragState

  pHandleKeyboard lh@LayersHandler {..} PotatoHandlerInput {..} kbd = case kbd of
    KeyboardData (KeyboardKey_Scroll scroll) _ -> r where
      scrollPos = _layersState_scrollPos _potatoHandlerInput_layersState
      maxentries = 10 + (Seq.length $ _layersState_entries _potatoHandlerInput_layersState)
      newScrollPos = max 0 (min maxentries (scrollPos + scroll))
      r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh
          -- TODO clamp based on number of entries
          , _potatoHandlerOutput_layersState = Just $ _potatoHandlerInput_layersState { _layersState_scrollPos = newScrollPos}
        }
    _ -> Nothing




  pRenderHandler lh@LayersHandler {..} PotatoHandlerInput {..} = if pIsHandlerActive lh
    then HandlerRenderOutput [LBox _layersHandler_cursorPos (V2 1 1)]
    else emptyHandlerRenderOutput
  pIsHandlerActive LayersHandler {..} = _layersHandler_dragState /= LDS_None
