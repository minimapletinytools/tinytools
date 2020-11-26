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

data LayerDownType = LDT_Hide | LDT_Lock | LDT_Normal deriving (Show, Eq)

-- TODO need scroll position
-- TODO rename this so it's like interactive state
type LayerDragState = Maybe (Int, LayerDownType)

clickLayer :: LayerIndents -> XY -> LayerDragState
clickLayer layers  (V2 absx absy) = case Seq.lookup absy layers of
  Nothing                      -> Nothing
  Just ident | ident == absx   -> Just (absy, LDT_Hide)
  Just ident | ident == absx+1 -> Just (absy, LDT_Lock)
  Just _                       -> Just (absy, LDT_Normal)

data LockShowOp = LockShowOp_Lock | LockShowOp_Unlock | LockShowOp_Show | LockShowOp_Hide deriving (Show)

layerInput :: LayerDragState -> LayerIndents -> MouseDrag -> (LayerDragState, Maybe PFEventTag, Maybe (LockShowOp, [Int]))
layerInput mlds indents md@MouseDrag {..} = let
    -- TODO handle scroll pos
    abspos = _mouseDrag_to
  in case _mouseDrag_state of
    MouseDragState_Down -> case mlds of
      Nothing -> (clickLayer indents abspos, Nothing, Nothing)
      _       -> error "unexpected"
    MouseDragState_Up -> case mlds of
      Nothing                    -> error "unexpected"
      Just (downInd, LDT_Normal) -> (Nothing, op, Nothing) where
        op = case clickLayer indents abspos of
          Nothing       -> Nothing
          Just (idx, _) -> undefined -- TODO drag to from, need PFState
      _ -> (Nothing, Nothing, Nothing)
    _ -> (mlds, Nothing, Nothing)
