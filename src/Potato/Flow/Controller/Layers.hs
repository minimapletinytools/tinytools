{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Controller.Layers (
  LayerDragState

  -- exposed for testing
  , LayerIndents
  , generateLayers
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
import  Data.Sequence                ((><))
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

data LockHiddenState = LHS_True | LHS_False | LHS_True_InheritTrue | LHS_False_InheritTrue deriving (Show)

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
    LHS_False -> LHS_False
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
  -- TODO rename to _layerEntry_depth
  _layerEntry_depth           :: Int

  -- TODO rename to lockState
  , _layerEntry_lockState :: LockHiddenState
  -- TODO rename to hiddenState
  , _layerEntry_hideState :: LockHiddenState
  , _layerEntry_isCollapsed :: Bool -- this parameter is ignored if not a folder, Maybe Bool instead?

  , _layerEntry_superSEltLabel :: SuperSEltLabel
} deriving (Show)

layerEntry_display :: LayerEntry -> Text
layerEntry_display LayerEntry {..} = label where
  (_,_,SEltLabel label _) = _layerEntry_superSEltLabel

layerEntry_isFolder :: LayerEntry -> Bool
layerEntry_isFolder LayerEntry {..} = case _layerEntry_superSEltLabel of
  (_,_,SEltLabel _ SEltFolderStart) -> True
  (_,_,SEltLabel _ SEltFolderEnd) -> True
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
addUntilFolderEndRecursive :: PFState -> LayerMetaMap -> (LayerMeta -> Bool) -> (SuperSEltLabel -> Int -> a) -> Bool -> Int -> LayerPos -> [a] -> (Int, [a])
addUntilFolderEndRecursive pfs@PFState {..} lmm skipfn eltfn skip depth lp added = let
    rid = Seq.index _pFState_layers lp
    seltl =  _pFState_directory IM.! rid
    sseltl = (rid, lp, seltl)
    selfEntry = eltfn sseltl depth
    combined = if skip then added else selfEntry:added
  in case seltl of
    SEltLabel _ SEltFolderStart -> if skipfn (lookupWithDefault rid lmm)
      then uncurry (addUntilFolderEndRecursive pfs lmm skipfn eltfn skip depth) $ addUntilFolderEndRecursive pfs lmm skipfn eltfn True (depth+1) (lp+1) combined
      -- recurse through children, and then continue where it left off
      else uncurry (addUntilFolderEndRecursive pfs lmm skipfn eltfn skip depth) $ addUntilFolderEndRecursive pfs lmm skipfn eltfn skip (depth+1) (lp+1) combined
    -- we're done!
    SEltLabel _ SEltFolderEnd -> (lp+1, added)
    -- nothing special, keep going
    _ -> addUntilFolderEndRecursive pfs lmm skipfn eltfn skip depth (lp+1) combined

-- see comments for addUntilFolderEndRecursive
addUntilFolderEnd :: PFState -> LayerMetaMap -> (LayerMeta -> Bool) -> (SuperSEltLabel -> Int -> a) -> Int -> Int -> Seq a
addUntilFolderEnd pfs lmm skipfn eltfn depth lp = Seq.fromList . reverse . snd $ addUntilFolderEndRecursive pfs lmm skipfn eltfn False depth lp []

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
  childles = Seq.takeWhileL childFrom . Seq.drop lepos $ lentries
  frontOfChildles = Seq.take lepos lentries
  backOfChildles = Seq.drop (lepos + Seq.length childles) lentries
  r = case op of
    LHCO_ToggleCollapse -> (newlmm, newlentries) where
      newcollapse = not $ _layerEntry_isCollapsed le
      newlmm = alterWithDefault (\le' -> le' { _layerMeta_isCollapsed = newcollapse }) lerid lmm

      startinglpos = layerEntry_layerPos le

      entryfn sseltl depth = LayerEntry {
          _layerEntry_depth = depth
          , _layerEntry_lockState = LHS_False
          , _layerEntry_hideState = LHS_False
          , _layerEntry_isCollapsed = True -- may not be folder, whatever
          , _layerEntry_superSEltLabel = sseltl
        }

      newchildles = addUntilFolderEnd pfs lmm _layerMeta_isCollapsed entryfn (_layerEntry_depth le + 1) lepos

      newlentries = if newcollapse
        then frontOfChildles >< backOfChildles
        else frontOfChildles >< newchildles >< backOfChildles

    LHCO_ToggleLock -> (newlmm, newlentries) where
      newlhsstate = toggleLockHiddenState $ _layerEntry_lockState le
      newlmm = alterWithDefault (\le' -> le' { _layerMeta_isLocked = lockHiddenStateToBool newlhsstate }) lerid lmm
      entryfn childle = childle { _layerEntry_lockState = updateLockHiddenStateInChildren newlhsstate (_layerEntry_lockState childle) }
      newchildles = doChildrenRecursive (lockHiddenStateToBool . _layerEntry_lockState) entryfn childles
      newlentries = frontOfChildles >< newchildles >< backOfChildles

    LHCO_ToggleHide -> (newlmm, newlentries) where
      -- TODO wrap this in helper fn to reduce code duplication from above?
      newlhsstate = toggleLockHiddenState $ _layerEntry_hideState le
      newlmm = alterWithDefault (\le' -> le' { _layerMeta_isHidden = lockHiddenStateToBool newlhsstate }) lerid lmm
      entryfn childle = childle { _layerEntry_hideState = updateLockHiddenStateInChildren newlhsstate (_layerEntry_hideState childle) }
      newchildles = doChildrenRecursive (lockHiddenStateToBool . _layerEntry_hideState) entryfn childles
      newlentries = frontOfChildles >< newchildles >< backOfChildles






-- TODO need to finish
-- by default
generateLayersNew :: PFState -> LayerMetaMap -> Seq LayerEntry
generateLayersNew PFState {..} lmm = Seq.fromList r where
  foldrfn rid lentries =  newlentry:lentries where
    seltl = case IM.lookup rid _pFState_directory of
      Nothing -> error "invalid PFState"
      Just x  -> x
    depth = case lentries of
      []  -> 0
      le:_ -> _layerEntry_depth le
    newDepth = case seltl of
      SEltLabel _ SEltFolderStart -> traceShow "-" $ depth - 1
      -- note that SEltFolderEnd is indented one level in from it's matching SEltFolderStart
      -- mainly so this function is simpler, it also doesn't matter since it's always hidden
      SEltLabel _ SEltFolderEnd   -> traceShow "+" $ depth + 1
      _ -> depth
    newlentry = LayerEntry {
        _layerEntry_depth = newDepth
        , _layerEntry_lockState = undefined
        , _layerEntry_hideState = undefined
        , _layerEntry_isCollapsed = undefined
        , _layerEntry_superSEltLabel = undefined
      }
  r = foldr foldrfn [] _pFState_layers






-- TODO delete
type LayerIndents = Seq Int

-- TODO delete
generateLayers :: PFState -> LayerIndents
generateLayers PFState {..} = Seq.fromList r where
  foldrfn rid idents =  newDepth:idents where
    seltl = case IM.lookup rid _pFState_directory of
      Nothing -> error "invalid PFState"
      Just x  -> x
    depth = case idents of
      []  -> 0
      x:_ -> x
    newDepth = case seltl of
      SEltLabel _ SEltFolderStart -> traceShow "-" $ depth - 1
      -- note that SEltFolderEnd is indented one level in from it's matching SEltFolderStart
      -- mainly so this function is simpler, it also doesn't matter since it's always hidden
      SEltLabel _ SEltFolderEnd   -> traceShow "+" $ depth + 1
      _ -> depth
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
