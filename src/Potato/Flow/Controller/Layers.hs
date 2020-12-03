{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Controller.Layers (
  LayerMeta(..)
  , LayerMetaMap

  -- exposed for testing
  , LockHiddenState(..)
  , LayerEntry(..)
  , LayerEntryPos
  , LockHideCollapseOp(..)
  , toggleLayerEntry
  , updateLayers
  , generateLayersNew


) where

import           Relude

import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Data.Aeson
import           Data.Default
import qualified Data.IntMap                  as IM
import           Data.Sequence                ((<|), (><), (|>))
import qualified Data.Sequence                as Seq
import           Data.Tuple.Extra



data LayerMeta = LayerMeta {
  -- if False, these will inherit from parent
  _layerMeta_isLocked      :: Bool
  , _layerMeta_isHidden    :: Bool
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
    _         -> LHS_False_InheritTrue
    LHS_True  -> LHS_False_InheritTrue
  True -> case parentstate of
    LHS_False -> LHS_True
    _         -> LHS_True_InheritTrue

-- ancestor state got set, update the child
updateLockHiddenStateInChildren :: LockHiddenState -> LockHiddenState -> LockHiddenState
updateLockHiddenStateInChildren parentstate = \case
  LHS_False -> case parentstate of
    LHS_True  -> LHS_False_InheritTrue
    LHS_False -> LHS_False
    _         -> invalid
  LHS_True -> case parentstate of
    LHS_True  -> LHS_True_InheritTrue
    LHS_False -> LHS_True
    _         -> invalid
  LHS_True_InheritTrue -> case parentstate of
    LHS_False -> LHS_True
    LHS_True  -> LHS_True_InheritTrue
    _         -> invalid
  LHS_False_InheritTrue -> case parentstate of
    LHS_False -> LHS_False
    LHS_True  -> LHS_False_InheritTrue
    _         -> invalid
  where
    invalid = error "toggling of LHS_XXX_InheritTrue elements disallowed"

-- this stores info just for what is displayed, Seq LayerEntry is uniquely generated from LayerMetaMap and PFState
data LayerEntry = LayerEntry {
  _layerEntry_depth            :: Int
  , _layerEntry_lockState      :: LockHiddenState
  , _layerEntry_hideState      :: LockHiddenState
  , _layerEntry_isCollapsed    :: Bool -- this parameter is ignored if not a folder, Maybe Bool instead?

  , _layerEntry_superSEltLabel :: SuperSEltLabel
} deriving (Eq, Show)

layerEntry_display :: LayerEntry -> Text
layerEntry_display LayerEntry {..} = label where
  (_,_,SEltLabel label _) = _layerEntry_superSEltLabel

layerEntry_isFolderStart :: LayerEntry -> Bool
layerEntry_isFolderStart LayerEntry {..} = case _layerEntry_superSEltLabel of
  (_,_,SEltLabel _ SEltFolderStart) -> True
  _                                 -> False

layerEntry_layerPos :: LayerEntry -> LayerPos
layerEntry_layerPos LayerEntry {..} = lp where
  (_,lp,_) = _layerEntry_superSEltLabel

layerEntry_rEltId :: LayerEntry -> REltId
layerEntry_rEltId LayerEntry {..} = rid where
  (rid,_,_) = _layerEntry_superSEltLabel

-- index type into Seq LayerEntry
type LayerEntryPos = Int

-- would have been smarter to store this as a Tree...
type LayerEntries = Seq LayerEntry

type LayerState = (LayerMetaMap, LayerEntries)

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
  Just x  -> x

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
  -> (LayerPos, [LayerEntry]) -- ^ (next lp, accumulator)
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
addUntilFolderEnd :: PFState -> LayerMetaMap -> (LayerMeta -> Bool) -> (SuperSEltLabel -> Maybe LayerEntry -> LayerEntry) -> Maybe LayerEntry -> LayerPos -> Seq LayerEntry
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

-- TODO change 'LayerMetaMap -> LayerEntries' to 'LayerState'
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
            Nothing     -> 0
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

-- updates lock and hide states (called after elements are added/removed from Seq LayerEntry)
-- i.e. fixes LayerEntry based on contents of LayerMetaMap
--updateLockHideState :: PFState -> LayerMetaMap -> Seq LayerEntry -> (LayerMetaMap, Seq LayerEntry)
--updateLockHideState = undefined


-- TODO change 'LayerMetaMap -> LayerEntries' to 'LayerState'
updateLayers :: PFState -> SEltLabelChanges -> LayerMetaMap -> LayerEntries -> LayerState
updateLayers pfs changes lmm lentries = r where
  -- update lmm
  (deletestuff, maybenewstuff) = IM.partition isNothing changes
  newlmm = IM.difference (IM.union lmm (fmap (const (def {_layerMeta_isCollapsed = True})) maybenewstuff)) deletestuff
  -- keep deleted elts so that folder state is preserved after undos/redos
  -- this is bad, because dragging creates a whole bunch of new elts right now...
    -- you could maybe fix this by changing the whole undoFirst thing to "undo permament" and not increase REltId counter
  --newlmm = IM.union lmm (fmap (const def) maybenewstuff)

  -- TODO incremental rather than regenerate...
  newlentries = generateLayersNew pfs newlmm

  r = (newlmm, newlentries)



-- TODO binary search instead
doesSelectionContainLayerPos :: LayerPos -> Selection -> Bool
doesSelectionContainLayerPos lp = isJust . find (\(_,lp',_) -> lp' == lp)

-- TODO delete LDT_Drag
data LayerDownType = LDT_Hide | LDT_Lock | LDT_Collapse | LDT_Normal | LDT_Drag deriving (Show, Eq)

clickLayerNew :: Selection -> Seq LayerEntry -> XY -> Maybe (LayerPos, LayerDownType)
clickLayerNew selection lentries  (V2 absx lepos) = case Seq.lookup lepos lentries of
  Nothing                      -> Nothing
  Just le -> Just . (,) lp $ case () of
    () | _layerEntry_depth le == absx+1   -> LDT_Hide
    () | _layerEntry_depth le == absx+2 -> LDT_Lock
    () | layerEntry_isFolderStart le && _layerEntry_depth le == absx -> LDT_Collapse
    ()                       -> LDT_Normal
    where
      lp = snd3 $ _layerEntry_superSEltLabel le

-- TODO consider supporting single click dragging, should be easy to do (drag select if you click off label, select+drag if you click on label)
data LayerDragState = LDS_None | LDS_Dragging | LDS_Selecting LayerEntryPos

layerInputNew ::
  PFState
  -> Int -- ^ scroll state
  -> LayerState
  -> LayerDragState
  -> Selection -- ^ current selection
  -> MouseDrag -- ^ input to update with
  -> (LayerDragState, LayerState, Maybe (Bool, LayerPos), Maybe PFEventTag)
layerInputNew pfs scrollPos layerstate@(lmm, lentries) lds selection md@MouseDrag {..} = let
    leposxy@(V2 _ lepos) = _mouseDrag_to + (V2 0 scrollPos)
  in case (_mouseDrag_state, lds) of
    (MouseDragState_Down, LDS_None) -> case clickLayerNew selection lentries leposxy of
      Nothing -> (LDS_None, layerstate, Nothing, Nothing)
      -- (you can only click + drag selected elements)
      Just (downlp, ldtdown) -> case ldtdown of
        LDT_Normal -> case doesSelectionContainLayerPos downlp selection of
          False -> (LDS_Selecting lepos, layerstate, Nothing, Nothing)
          True  -> (LDS_Dragging, layerstate, Nothing, Nothing)
        LDT_Hide -> (LDS_None, toggleLayerEntry pfs lmm lentries undefined LHCO_ToggleHide, Nothing, Nothing)
        LDT_Lock -> (LDS_None, toggleLayerEntry pfs lmm lentries undefined LHCO_ToggleLock, Nothing, Nothing)
        LDT_Collapse -> (LDS_None, toggleLayerEntry pfs lmm lentries undefined LHCO_ToggleCollapse, Nothing, Nothing)
    (MouseDragState_Down, _)       -> error "unexpected, LayerDragState should have been reset on last mouse up"
    (MouseDragState_Up, LDS_None) -> error "unexpected, layer input handler should not have been created"
    -- TODO support drag selecting
    (MouseDragState_Up, LDS_Selecting leposdown) -> (LDS_None, layerstate, Just (shift, selectlp), Nothing) where
      shift = elem KeyModifier_Shift _mouseDrag_modifiers
      selectlp = snd3 . _layerEntry_superSEltLabel $ Seq.index lentries leposdown
    (MouseDragState_Up, LDS_Dragging) -> case clickLayerNew selection lentries leposxy of
      -- release where there is no element, do nothing
      Nothing -> (LDS_None, layerstate, Nothing, Nothing)
      Just (uplp,_) -> case doesSelectionContainLayerPos uplp selection of
        -- dropping on a selected element does onthing
        True ->  (LDS_None, layerstate, Nothing, Nothing)
        False -> (LDS_None, layerstate, Nothing, Just $ PFEMoveElt (toList (fmap snd3 selection), uplp))

    -- TODO make sure this is the right way to cancel...
    (MouseDragState_Cancelled, _) -> (LDS_None, layerstate, Nothing, Nothing)
    -- continue
    _ -> (lds, layerstate, Nothing, Nothing)
