{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Controller.Layers (
  LayerDragState
  , LayerMeta(..)
  , LayerMetaMap

  -- exposed for testing
  , LayerIndents

  , LockHiddenState(..)
  , LayerEntry(..)
  , LayerEntryPos
  , LockHideCollapseOp(..)
  , toggleLayerEntry
  , generateLayersNew

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
import  Data.Sequence                ((|>), (<|), (><))
import Data.Tuple.Extra
import           Data.Aeson
import Data.Default



data LayerMeta = LayerMeta {
  -- if False, these will inherit from parent
  _layerMeta_isLocked :: Bool
  , _layerMeta_isHidden :: Bool
  , _layerMeta_isCollapsed :: Bool

} deriving (Eq, Generic, Show)

instance FromJSON LayerMeta
instance ToJSON LayerMeta
instance NFData LayerMeta

instance Default LayerMeta where
  def = LayerMeta {
      _layerMeta_isLocked = False
      , _layerMeta_isHidden = False
      , _layerMeta_isCollapsed = True
    }

type LayerMetaMap = REltIdMap LayerMeta

data LockHiddenState = LHS_True | LHS_False | LHS_True_InheritTrue | LHS_False_InheritTrue deriving (Eq, Show)

lockHiddenStateToBool :: LockHiddenState -> Bool
lockHiddenStateToBool = \case
  LHS_False -> False
  _ -> True

toggleLockHiddenState :: LockHiddenState -> LockHiddenState
toggleLockHiddenState = \case
  LHS_True -> LHS_False
  LHS_False -> LHS_True
  LHS_True_InheritTrue -> LHS_False_InheritTrue
  LHS_False_InheritTrue -> LHS_True_InheritTrue

setLockHiddenStateInChildren :: LockHiddenState -> Bool -> LockHiddenState
setLockHiddenStateInChildren parentstate = \case
  False -> case parentstate of
    LHS_False -> LHS_False
    _ -> LHS_False_InheritTrue
    LHS_True -> LHS_False_InheritTrue
  True -> case parentstate of
    LHS_False -> LHS_True
    _ -> LHS_True_InheritTrue

-- ancestor state got set, update the child
updateLockHiddenStateInChildren :: LockHiddenState -> LockHiddenState -> LockHiddenState
updateLockHiddenStateInChildren parentstate = \case
  LHS_False -> case parentstate of
    LHS_True -> LHS_False_InheritTrue
    LHS_False -> LHS_False
    _ -> invalid
  LHS_True -> case parentstate of
    LHS_True -> LHS_True_InheritTrue
    LHS_False -> LHS_True
    _ -> invalid
  LHS_True_InheritTrue -> case parentstate of
    LHS_False -> LHS_True
    LHS_True -> LHS_True_InheritTrue
    _ -> invalid
  LHS_False_InheritTrue -> case parentstate of
    LHS_False -> LHS_False
    LHS_True -> LHS_False_InheritTrue
    _ -> invalid
  where
    invalid = error "toggling of LHS_XXX_InheritTrue elements disallowed"

-- this stores info just for what is displayed, Seq LayerEntry is uniquely generated from LayerMetaMap and PFState
data LayerEntry = LayerEntry {
  _layerEntry_depth           :: Int
  , _layerEntry_lockState :: LockHiddenState
  , _layerEntry_hideState :: LockHiddenState
  , _layerEntry_isCollapsed :: Bool -- this parameter is ignored if not a folder, Maybe Bool instead?

  , _layerEntry_superSEltLabel :: SuperSEltLabel
} deriving (Eq, Show)

layerEntry_display :: LayerEntry -> Text
layerEntry_display LayerEntry {..} = label where
  (_,_,SEltLabel label _) = _layerEntry_superSEltLabel

layerEntry_isFolderStart :: LayerEntry -> Bool
layerEntry_isFolderStart LayerEntry {..} = case _layerEntry_superSEltLabel of
  (_,_,SEltLabel _ SEltFolderStart) -> True
  _ -> False

layerEntry_layerPos :: LayerEntry -> LayerPos
layerEntry_layerPos LayerEntry {..} = lp where
  (_,lp,_) = _layerEntry_superSEltLabel

layerEntry_rEltId :: LayerEntry -> REltId
layerEntry_rEltId LayerEntry {..} = rid where
  (rid,_,_) = _layerEntry_superSEltLabel

-- index type into Seq LayerEntry
type LayerEntryPos = Int

data LockHideCollapseOp = LHCO_ToggleLock | LHCO_ToggleHide | LHCO_ToggleCollapse deriving (Show)


alterWithDefault :: (Eq a, Default a) => (a -> a) -> REltId -> REltIdMap a -> REltIdMap a
alterWithDefault f k m = IM.alter f' k m where
  apply x = if fx == def then Nothing else Just fx where
    fx = f x
  f' = \case
    Nothing -> apply def
    Just x -> apply x

lookupWithDefault :: (Default a) => REltId -> REltIdMap a -> a
lookupWithDefault rid ridm = case IM.lookup rid ridm of
  Nothing -> def
  Just x -> x

-- iterates over LayerPos not LayerEntryPos
-- assumes folder start has already been processed and we are processing its children
-- very partial, assumes state is valid!!!
-- assembles list backwards, remember to reverse results when you're done!

-- TODO change depth to Maybe LayerEntry? (parent layer entry)
addUntilFolderEndRecursive ::
  PFState
  -> LayerMetaMap
  -> (LayerMeta -> Bool)
  -> (SuperSEltLabel -> Maybe LayerEntry -> LayerEntry)
  -> Bool
  -> Maybe LayerEntry -- ^ parent
  -> LayerPos -- ^ current layer position we are adding
  -> [LayerEntry] -- ^ accumulator
  -> (Int, [LayerEntry])
addUntilFolderEndRecursive pfs@PFState {..} lmm skipfn eltfn skip parent lp added = let
    rid = Seq.index _pFState_layers lp
    seltl =  _pFState_directory IM.! rid
    sseltl = (rid, lp, seltl)
    selfEntry = eltfn sseltl parent
    combined = if skip then added else selfEntry:added
  in if lp >= Seq.length _pFState_layers
    -- this means we've reached the end of layers, nothing to do
    then (lp+1, added)
    -- normal case
    else case seltl of
      SEltLabel _ SEltFolderStart -> if not skip && skipfn (lookupWithDefault rid lmm)
          -- recurse through children (skipping) and the continue where it left off
          then uncurry (addUntilFolderEndRecursive pfs lmm skipfn eltfn skip parent) $ addUntilFolderEndRecursive pfs lmm skipfn eltfn True (Just selfEntry) (lp+1) combined
          -- recurse through children (possibly skipping), and then continue where it left off
          else uncurry (addUntilFolderEndRecursive pfs lmm skipfn eltfn skip parent) $ addUntilFolderEndRecursive pfs lmm skipfn eltfn skip (Just selfEntry) (lp+1) combined
      -- we're done!
      SEltLabel _ SEltFolderEnd -> (lp+1, added)
      -- nothing special, keep going
      _ ->addUntilFolderEndRecursive pfs lmm skipfn eltfn skip parent (lp+1) combined

-- see comments for addUntilFolderEndRecursive
addUntilFolderEnd :: PFState -> LayerMetaMap -> (LayerMeta -> Bool) -> (SuperSEltLabel -> Maybe LayerEntry -> LayerEntry) -> Maybe LayerEntry -> Int -> Seq LayerEntry
addUntilFolderEnd pfs lmm skipfn eltfn parent lp = Seq.fromList . reverse . snd $ addUntilFolderEndRecursive pfs lmm skipfn eltfn False parent lp []

-- iterates over LayerEntryPos, skipping all children of entries where skipfn evaluates to true
doChildrenRecursive :: (LayerEntry -> Bool) -> (LayerEntry -> LayerEntry) -> Seq LayerEntry -> Seq LayerEntry
doChildrenRecursive skipfn entryfn = snd . mapAccumL mapaccumlfn maxBound where
  mapaccumlfn skipdepth le = (newskipdepth, newle) where
    depth = _layerEntry_depth le
    newskipdepth
      -- skip, so keep skipping
      | depth >= skipdepth = skipdepth
      -- skip all children
      -- note, no need to check for collapsed state because we are iterating over LayerEntry which do not include children of collapsed entries
      | skipfn le = depth + 1
      -- either we exited a skipped folder or aren't skipping, reset skip counter (since we skip subfolders of skipped entries, maximal skip stack depth is 1 so reset is OK)
      | depth < skipdepth = maxBound
    newle = if depth >= skipdepth
      then le -- no changes to skipped elts
      else entryfn le

toggleLayerEntry :: PFState -> LayerMetaMap -> Seq LayerEntry -> LayerEntryPos -> LockHideCollapseOp -> (LayerMetaMap, Seq LayerEntry)
toggleLayerEntry pfs@PFState {..} lmm lentries lepos op = r where
  le = Seq.index lentries lepos
  ledepth = _layerEntry_depth le
  lerid = Seq.index _pFState_layers lepos
  childFrom nextLayerEntry = _layerEntry_depth nextLayerEntry /= ledepth
  -- visible children of le
  childles = Seq.takeWhileL childFrom . Seq.drop (lepos+1) $ lentries
  -- everything before le
  frontOfLe = Seq.take lepos lentries
  -- everything after childles
  backOfChildles = Seq.drop (lepos + 1 + Seq.length childles) lentries

  togglefn fn setfn = (newlmm, newlentries) where
    newlhsstate = toggleLockHiddenState $ fn le
    newlmm = alterWithDefault (\le' -> le' { _layerMeta_isHidden = lockHiddenStateToBool newlhsstate }) lerid lmm
    entryfn childle = setfn childle $ updateLockHiddenStateInChildren newlhsstate (fn childle)
    newchildles = doChildrenRecursive (lockHiddenStateToBool . fn) entryfn childles
    newle = setfn le newlhsstate
    newlentries = (frontOfLe |> newle) >< newchildles >< backOfChildles

  r = case op of
    LHCO_ToggleCollapse -> (newlmm, newlentries) where
      newcollapse = not $ _layerEntry_isCollapsed le
      newlmm = alterWithDefault (\le' -> le' { _layerMeta_isCollapsed = newcollapse }) lerid lmm

      startinglpos = layerEntry_layerPos le

      entryfn sseltl mparent = LayerEntry {
          _layerEntry_depth = case mparent of
            Just parent -> _layerEntry_depth parent + 1
            Nothing -> 0
          , _layerEntry_lockState = LHS_False
          , _layerEntry_hideState = LHS_False
          , _layerEntry_isCollapsed = True -- may not be folder, whatever
          , _layerEntry_superSEltLabel = sseltl
        }

      newle = le { _layerEntry_isCollapsed = newcollapse }
      newchildles = addUntilFolderEnd pfs lmm _layerMeta_isCollapsed entryfn (Just le) (startinglpos+1)

      newlentries = if newcollapse
        then (frontOfLe |> newle) >< backOfChildles
        else (frontOfLe |> newle) >< newchildles >< backOfChildles

    LHCO_ToggleLock -> togglefn _layerEntry_lockState (\le' x -> le' { _layerEntry_lockState = x })
    LHCO_ToggleHide -> togglefn _layerEntry_hideState (\le' x -> le' { _layerEntry_hideState = x })




generateLayersNew :: PFState -> LayerMetaMap -> Seq LayerEntry
generateLayersNew pfs lmm = r where
  entryfn sseltl mparent = case mparent of
    Nothing -> LayerEntry {
      _layerEntry_depth        =  0
      , _layerEntry_lockState =  if _layerMeta_isLocked lm then LHS_True else LHS_False
      , _layerEntry_hideState = if _layerMeta_isHidden lm then LHS_True else LHS_False
      , _layerEntry_isCollapsed = _layerMeta_isCollapsed lm
      , _layerEntry_superSEltLabel = sseltl
    }
    Just parent -> LayerEntry {
      _layerEntry_depth        = _layerEntry_depth parent + 1
      , _layerEntry_lockState = setLockHiddenStateInChildren (_layerEntry_lockState parent) $ _layerMeta_isLocked lm
      , _layerEntry_hideState = setLockHiddenStateInChildren (_layerEntry_hideState parent) $ _layerMeta_isHidden lm
      , _layerEntry_isCollapsed = _layerMeta_isCollapsed lm
      , _layerEntry_superSEltLabel = sseltl
    }
    where
      lm = lookupWithDefault (fst3 sseltl) lmm
  r = addUntilFolderEnd pfs lmm (_layerMeta_isCollapsed) entryfn Nothing 0





-- TODO delete
type LayerIndents = Seq Int





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


-- TODO layer indexing is wrong, you need a function to convert display layer pos (abspos) to true layer pos due to hidden stuff
-- TODO binary search instead
doesSelectionContainLayerPos :: LayerPos -> Selection -> Bool
doesSelectionContainLayerPos lp = isJust . find (\(_,lp',_) -> lp' == lp)

-- has collapsed folders and hides SEltFolderEnds
data CompactLayers = CompactLayers {

}

-- TODO
convertToTrueIndex :: Int -> CompactLayers -> LayerPos
convertToTrueIndex absy cl = undefined

-- TODO layer indexing is wrong, you need a function to convert display layer pos (abspos) to true layer pos due to hidden stuff
-- TODO add CompactLayers arg
-- TODO add selection output
layerInput :: PFState -> Int -> Selection -> LayerDragState -> LayerIndents -> MouseDrag -> (LayerDragState, Maybe PFEventTag, Maybe (LockHideCollapseOp, [Int]))
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
