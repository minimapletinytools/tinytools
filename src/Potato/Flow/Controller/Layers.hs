{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Layers (
  LayerDragState
) where

import           Relude

import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import qualified Data.IntMap                  as IM
import qualified Data.Sequence                as Seq
import Data.Tuple.Extra

-- DELETE prob
data LayerEntry = LayerEntry {
  _layerEntry_indent           :: Int
  , _layerEntry_display        :: Text
  , _layerEntry_isFolder       :: Bool

  -- stuff above is redundant if we have this but whatever
  , _layerEntry_superSEltLabel :: SuperSEltLabel

}

type LayerIndents = Seq Int

generateLayers :: PFState -> LayerIndents
generateLayers PFState {..} = Seq.fromList r where
  foldrfn rid idents = newDepth:idents where
    seltl = case IM.lookup rid _pFState_directory of
      Nothing -> error "invalid PFState"
      Just x  -> x
    depth = case idents of
      []  -> 0
      x:_ -> x
    newDepth = case seltl of
      SEltLabel _ SEltFolderStart -> depth - 1
      SEltLabel _ SEltFolderEnd   -> depth + 1
  r = foldr foldrfn [] _pFState_layers

data LayerDownType = LDT_Hide | LDT_Lock | LDT_Normal | LDT_Drag deriving (Show, Eq)

-- TODO rename this so it's like interactive state
type LayerDragState = Maybe (Int, LayerDownType)

-- TODO layer indexing is wrong, you need a function to convert display layer pos (abspos) to true layer pos due to hidden stuff
clickLayer :: Selection -> LayerIndents -> XY -> LayerDragState
clickLayer selection layers  (V2 absx absy) = case Seq.lookup absy layers of
  Nothing                      -> Nothing
  Just ident | ident == absx   -> Just (absy, LDT_Hide)
  Just ident | ident == absx+1 -> Just (absy, LDT_Lock)
  Just _                       -> Just (absy, ldttype) where
    ldttype = if doesSelectionContainLayerPos absy selection
      then LDT_Drag
      else LDT_Normal

data LockShowOp = LockShowOp_Lock | LockShowOp_Unlock | LockShowOp_Show | LockShowOp_Hide deriving (Show)

-- TODO layer indexing is wrong, you need a function to convert display layer pos (abspos) to true layer pos due to hidden stuff
-- TODO binary search instead
doesSelectionContainLayerPos :: LayerPos -> Selection -> Bool
doesSelectionContainLayerPos lp = isJust . find (\(_,lp',_) -> lp' == lp)

-- TODO layer indexing is wrong, you need a function to convert display layer pos (abspos) to true layer pos due to hidden stuff
layerInput :: PFState -> Int -> Selection -> LayerDragState -> LayerIndents -> MouseDrag -> (LayerDragState, Maybe PFEventTag, Maybe (LockShowOp, [Int]))
layerInput PFState {..} scrollPos selection mlds indents md@MouseDrag {..} = let
    abspos@(V2 _ absy) = _mouseDrag_to + (V2 0 scrollPos)
    selectionInds = fmap snd3 selection
  in case _mouseDrag_state of
    MouseDragState_Down -> case mlds of
      Nothing -> (clickLayer selection indents abspos, Nothing, Nothing) where
      _       -> error "unexpected"
    MouseDragState_Up -> let
        mldsnew = clickLayer selection indents abspos
      in case mlds of
        Nothing                    -> error "unexpected"
        -- this means we clicked down on something selected and want to drag it
        Just (downInd, LDT_Drag) -> (Nothing, op, Nothing) where
          op = case mldsnew of
            Nothing       -> Nothing
            Just (idx, LDT_Drag) -> Nothing -- this means we released on something we already selected, don't do anything
            Just (idx, _) -> if doesSelectionContainLayerPos absy selection -- we have to check again because LDT_Hide/Lock override LDT_Drag :(
              then Nothing -- this means we released on something we already selected, don't do anything
              else Just $ PFEMoveElt (toList (fmap snd3 selection), idx)
        Just (downInd, oldldt) -> case mldsnew of
          Nothing -> (Nothing, Nothing, Nothing)
          Just (idx, _) -> case oldldt of
            LDT_Normal -> if downInd == idx
              then undefined -- TODO select/shift select
              else (Nothing, Nothing, Nothing) -- maybe we can do something cute here instead?

            LDT_Hide -> if downInd == idx
              then (Nothing, Nothing, Just (showop, [downInd]))
              else (Nothing, Nothing, Nothing) -- TODO consider click drag to Show/Hide several things at once (depending on if first thing clicked is shown/hidden)
                where
                    showop = undefined -- TODO showop depends on what original state was
            LDT_Lock -> if downInd == idx
              then (Nothing, Nothing, Just (lockop, [downInd]))
              else (Nothing, Nothing, Nothing) -- TODO consider click drag to Show/Hide several things at once (depending on if first thing clicked is shown/hidden)
                where
                  lockop = undefined -- TODO showop depends on what original state was
        _ -> (Nothing, Nothing, Nothing)
    _ -> (mlds, Nothing, Nothing)
