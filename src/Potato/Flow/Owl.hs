{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Owl where

import Relude
import qualified Relude.Unsafe as Unsafe

import Control.Exception (assert)
import Data.Foldable (foldl)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Sequence ((><), (|>), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import qualified Data.Text as T
import Potato.Flow.OwlItem
import Potato.Flow.Serialization.Snake
import Potato.Flow.Types
import Potato.Flow.DebugHelpers

errorMsg_owlTree_lookupFail :: OwlTree -> REltId -> Text
errorMsg_owlTree_lookupFail OwlTree {..} rid = errorMsg_owlMapping_lookupFail _owlTree_mapping rid

errorMsg_owlMapping_lookupFail :: OwlMapping -> REltId -> Text
errorMsg_owlMapping_lookupFail _ rid = "expected to find REltId " <> show rid <> " in OwlMapping"

type OwlMapping = REltIdMap (OwlItemMeta, OwlItem)

-- | update attachments based on remap
owlItem_updateAttachments :: Bool -> REltIdMap REltId -> OwlItem -> OwlItem
owlItem_updateAttachments breakNonExistng ridremap oitem = case oitem of
  OwlItem oinfo (OwlSubItemLine sline) -> OwlItem oinfo $ OwlSubItemLine (sline {
      _sAutoLine_attachStart = remapAttachment $ _sAutoLine_attachStart sline
      , _sAutoLine_attachEnd = remapAttachment $ _sAutoLine_attachEnd sline
    })
    where
      remapAttachment ma = case ma of
        Nothing -> Nothing
        Just a -> case IM.lookup (_attachment_target a) ridremap of
          -- could not find attachment, break it
          Nothing -> if breakNonExistng then Nothing else Just a
          Just t -> Just $ a { _attachment_target = t }
  x -> x



-- this is just position index in children
type SiblingPosition = Int

-- TODO remove OwlMapping arg not needed
locateLeftSiblingIdFromSiblingPosition :: OwlMapping -> Seq REltId -> SiblingPosition -> Maybe REltId
locateLeftSiblingIdFromSiblingPosition _ s sp = case sp of
  0 -> Nothing
  x -> case Seq.lookup (x - 1) s of
    Nothing -> error $ "expected to find index " <> show (x - 1) <> " in seq"
    Just r -> Just r

 
-- TODO
--isAncestorOf

isDescendentOf :: (HasCallStack) => OwlMapping -> REltId -> REltId -> Bool
isDescendentOf om parent child
  | child == noOwl = False
  | otherwise = r
  where
    parent' = case IM.lookup child om of
      Just (oem, _) -> _owlItemMeta_parent oem
      Nothing -> error $ errorMsg_owlMapping_lookupFail om child
    r = case parent' of
      x | x == noOwl -> False
      x | x == parent -> True
      x -> isDescendentOf om parent x

data OwlItemMeta = OwlItemMeta
  { _owlItemMeta_parent :: REltId
    , _owlItemMeta_depth :: Int
    , _owlItemMeta_position :: SiblingPosition
  }
  deriving (Eq, Show, Generic)

instance NFData OwlItemMeta

instance PotatoShow OwlItemMeta where
  potatoShow OwlItemMeta {..} = "(meta: " <> show _owlItemMeta_parent <> " " <> show _owlItemMeta_depth <> " " <> show _owlItemMeta_position <> ")"

-- a simpler version of OwlItemMeta used for inserting new Owls
data OwlSpot = OwlSpot {
    -- NOTE _owlSpot_parent is redundant if _owlSpot_leftSibling is not Nothing
    _owlSpot_parent :: REltId,
    _owlSpot_leftSibling :: Maybe REltId
  }
  deriving (Show, Generic)

instance NFData OwlSpot

topSpot :: OwlSpot
topSpot = OwlSpot noOwl Nothing

-- TODO try and get rid of deriving Eq
data SuperOwl = SuperOwl
  { _superOwl_id :: REltId,
    _superOwl_meta :: OwlItemMeta,
    _superOwl_elt :: OwlItem
  }
  deriving (Eq, Show, Generic)

-- TODO something like
--type SuperDuperOwl = (SuperOwl, OwlTree)
-- or even data Duper a = Duper OwlTree a

instance NFData SuperOwl

instance MommyOwl SuperOwl where
  mommyOwl_kiddos sowl = mommyOwl_kiddos (_superOwl_elt sowl)

instance HasOwlItem SuperOwl where
  hasOwlItem_owlItem = _superOwl_elt


type SuperOwlChanges = REltIdMap (Maybe SuperOwl)

-- updates AttachmeentMap with a list of SuperOwls (that may be attached to stuff)
attachmentMap_addSuperOwls' :: (Foldable f) => (Attachment -> Bool) -> f SuperOwl -> AttachmentMap -> AttachmentMap
attachmentMap_addSuperOwls' filterfn sowls am = r where
  foldrfn sowl m = newmap where
    --find all targets we are attached to
    attachedstuff = filter filterfn (hasOwlItem_attachments sowl)
    alterfn stuff ms = Just $ case ms of
      Nothing -> (IS.singleton stuff)
      Just s -> IS.insert stuff s
    innerfoldrfn target m' = IM.alter (alterfn (_superOwl_id sowl)) target m'
    newmap = foldr innerfoldrfn m (fmap _attachment_target attachedstuff)
  r = foldr foldrfn am sowls

attachmentMap_addSuperOwls :: (Foldable f) => f SuperOwl -> AttachmentMap -> AttachmentMap
attachmentMap_addSuperOwls = attachmentMap_addSuperOwls' (const True)

-- TODO test I have no idea if I did this right...
-- | update AttachmentMap from SuperOwlChanges (call on SuperOwlChanges produced by updateOwlPFWorkspace)
updateAttachmentMapFromSuperOwlChanges :: SuperOwlChanges -> AttachmentMap -> AttachmentMap
updateAttachmentMapFromSuperOwlChanges changes am = newam_4 where

  -- remove deleted stuff from keys
  --newam_1 = foldr (\k acc -> IM.delete k acc) am $ IM.keys (IM.filter isNothing changes)
  -- actually don't bother
  newam_1 = am

  -- remove changed elems from all value sets (this could be done more efficiently if we know the previous things they were attached to, but oh well)
  setToRemove = IM.keysSet changes
  newam_2 = IM.filter (not . IS.null) $ fmap (\s -> IS.difference s setToRemove) newam_1

  -- add attachment targets of changed elems to value sets of those targets
  justChanges = catMaybes . IM.elems $ changes
  newam_3 = attachmentMap_addSuperOwls justChanges newam_2

  -- needing to iterate through everything when there are newly created elts is kind of unfortunate :(. Especially when this is only meeaningful in undo cases. probably not worth trying to optimize away. I guess we could keep deleted elts around in AttachmentMap for some time?
  --sowls = owliterateall ot
  --newam_4 = if IS.null newstuff then newam_3 else attachmentMap_addSuperOwls' (\x -> IS.member (_attachment_target x) newstuff) sowls newam_3
  -- similarly, since we skip computing newam_1, we can skip computing newam_4
  newam_4 = newam_3

-- | update SuperOwlChanges to include stuff attached to stuff that changed (call before rendering)
getChangesFromAttachmentMap :: OwlTree -> AttachmentMap -> SuperOwlChanges -> SuperOwlChanges
getChangesFromAttachmentMap owltreeafterchanges am changes = r where
  -- collect all stuff attaching to changed stuff
  changeset = IS.unions . catMaybes $ foldr (\k acc -> IM.lookup k am : acc) [] (IM.keys changes)

  -- create SuperOwlChanges from changeset
  -- currently nothing can be attached to something that is attaching to thing sso you don't need to make this operation recursive
  r = IM.fromList . filter (\(_,x) -> isJust x) . fmap (\rid -> (rid, owlTree_findSuperOwl owltreeafterchanges rid)) .  IS.toList $ changeset

instance PotatoShow SuperOwl where
  potatoShow SuperOwl {..} = show _superOwl_id <> " " <> potatoShow _superOwl_meta <> " " <> elt
    where
      elt = potatoShow _superOwl_elt

        --case _superOwl_elt of
          --OwlItem oinfo (OwlSubItemFolder kiddos) -> "folder: " <> (_owlInfo_name oinfo) <> ": " <> show kiddos
          --OwlItem oinfo _ -> "elt: " <> (_owlInfo_name oinfo) -- TODO elt type

--superOwl_id :: Lens' SuperOwl REltId
superOwl_id :: Functor f => (REltId -> f REltId) -> SuperOwl -> f SuperOwl
superOwl_id f sowl = fmap (\rid -> sowl {_superOwl_id = rid}) (f (_superOwl_id sowl))

-- TODO rest of lenses

superOwl_isTopOwl :: SuperOwl -> Bool
superOwl_isTopOwl SuperOwl {..} = _owlItemMeta_depth _superOwl_meta == 0

-- | same as superOwl_isTopOwl except checks all conditions, intended to be used in asserts
superOwl_isTopOwlSurely :: SuperOwl -> Bool
superOwl_isTopOwlSurely SuperOwl {..} = _owlItemMeta_depth _superOwl_meta == 0 && _owlItemMeta_parent _superOwl_meta == noOwl

noOwl :: REltId
noOwl = -1

superOwl_parentId :: SuperOwl -> REltId
superOwl_parentId SuperOwl {..} = _owlItemMeta_parent _superOwl_meta

superOwl_depth :: SuperOwl -> Int
superOwl_depth SuperOwl {..} = _owlItemMeta_depth _superOwl_meta

superOwl_owlSubItem :: SuperOwl -> OwlSubItem
superOwl_owlSubItem sowl = case _superOwl_elt sowl of
  OwlItem _ x -> x


owlTree_superOwlNthParentId :: OwlTree -> SuperOwl -> Int -> REltId
owlTree_superOwlNthParentId _ sowl 0 = _superOwl_id sowl
owlTree_superOwlNthParentId od sowl n
  | superOwl_parentId sowl == noOwl = noOwl
  | otherwise = owlTree_superOwlNthParentId od (owlTree_mustFindSuperOwl od (superOwl_parentId sowl)) (n-1)

-- if parent is selected, then kiddos must not be directly included in the parliament
newtype OwlParliament = OwlParliament {unOwlParliament :: Seq REltId} deriving (Show, Generic)

instance NFData OwlParliament

-- same as OwlParialment but contains more information
-- TODO consider adding OwlTree reference to this type and rename to SuperDuperOwlParliament or something like that
newtype SuperOwlParliament = SuperOwlParliament {unSuperOwlParliament :: Seq SuperOwl} deriving (Eq, Show, Generic)

instance NFData SuperOwlParliament

instance PotatoShow SuperOwlParliament where
  potatoShow (SuperOwlParliament sowls) = T.intercalate "\n" . toList $ fmap potatoShow sowls

class IsParliament a where
  isParliament_disjointUnion :: a -> a -> a
  isParliament_null :: a -> Bool
  isParliament_empty :: a
  isParliament_length :: a -> Int

--  isParliament_isValid :: OwlMapping -> a -> Bool

disjointUnion :: (Eq a) => [a] -> [a] -> [a]
disjointUnion a b = L.union a b L.\\ L.intersect a b

instance IsParliament OwlParliament where
  isParliament_disjointUnion (OwlParliament s1) (OwlParliament s2) = OwlParliament $ Seq.fromList $ disjointUnion (toList s1) (toList s2)
  isParliament_null = Seq.null . unOwlParliament
  isParliament_empty = OwlParliament Seq.empty
  isParliament_length (OwlParliament x) = Seq.length x


instance IsParliament SuperOwlParliament where
  isParliament_disjointUnion (SuperOwlParliament s1) (SuperOwlParliament s2) = SuperOwlParliament $ Seq.fromList $ disjointUnion (toList s1) (toList s2)
  isParliament_null = Seq.null . unSuperOwlParliament
  isParliament_empty = SuperOwlParliament Seq.empty
  isParliament_length (SuperOwlParliament x) = Seq.length x

owlParliament_toSuperOwlParliament :: OwlTree -> OwlParliament -> SuperOwlParliament
owlParliament_toSuperOwlParliament od@OwlTree {..} op = SuperOwlParliament $ fmap f (unOwlParliament op)
  where
    f rid = case IM.lookup rid _owlTree_mapping of
      Nothing -> error $ errorMsg_owlTree_lookupFail od rid
      Just (oem, oe) -> SuperOwl rid oem oe

superOwlParliament_toOwlParliament :: SuperOwlParliament -> OwlParliament
superOwlParliament_toOwlParliament = OwlParliament . fmap _superOwl_id . unSuperOwlParliament


-- | partition a list into groups based on int pairings
partitionN :: (a -> Int) -> Seq a -> IM.IntMap (Seq a)
partitionN f as = r where
  alterfn x ml = case ml of
    Nothing -> Just (Seq.singleton x)
    Just xs -> Just (x<|xs)
  foldfn a acc = IM.alter (alterfn a) (f a) acc
  r = foldr foldfn IM.empty as

-- TODO how is this different than `\od sowls -> Seq.sortBy (owlTree_superOwl_comparePosition od) sowls`
  -- if it's not, than you can use them to UT against each other
-- TODO rename, SuperOwlParliament is always sorted so the name is redundant!
-- input type is not SuperOwlParliament type because it is not ordered
makeSortedSuperOwlParliament :: OwlTree -> Seq SuperOwl -> SuperOwlParliament
makeSortedSuperOwlParliament od sowls = r where

  -- attach parents (at front of list, last elt is child and actuall part of original selection)
  makeParentChain :: SuperOwl -> [SuperOwl]
  makeParentChain sowl = done where
    makeParentChain' sowl' acc = case superOwl_parentId sowl' of
      x | x == noOwl -> acc
      x -> makeParentChain' parentsowl (parentsowl:acc) where
        parentsowl = owlTree_mustFindSuperOwl od x
    done = makeParentChain' sowl (sowl:[])

  parentChains = fmap makeParentChain sowls

  -- this function is sketch af D:
  sortrec :: Seq [SuperOwl] -> Seq SuperOwl
  sortrec chains = done where
    frontid (x:_) = _superOwl_id x
    frontid _ = error "should never happen"

    groupedParentChains = partitionN frontid chains

    removeFront (_:xs) = xs
    removeFront [] = error "should never happen"

    -- it's not necessary to look up rid as it will be the first element in each Seq elt in the value but whatever this is easier (to fix, you should rewrite partitionN)
    groupedParentChains2 = fmap (\(rid,x) -> (owlTree_mustFindSuperOwl od rid, x)) . Seq.fromList . IM.toList $ groupedParentChains
    cfn = _owlItemMeta_position . _superOwl_meta . fst
    sortedPairs = Seq.sortOn cfn $ groupedParentChains2

    -- sketchy logic here reliant on assumptions carried over from previous iteration... TODO rewrite this so it's not so weird
    fmapfn (_, chains') = if Seq.length chains' == 1
      -- this is unititive, but if the group has only 1 chain in it, that means it's already sorted and hence are leaf node case
      then join $ fmap (Seq.singleton . Unsafe.last) chains'
      -- otherwise, we have more children to process, note that if assumptions are correct, then each chain in the sequence has at least 2 elts (otherwise it would have been caught by the above condition in the previous iteration)
      else sortrec (fmap removeFront chains')


    done = join . fmap fmapfn  $ sortedPairs

  r = SuperOwlParliament $ sortrec parentChains

-- TODO test
-- assumes s1 is and s2 are valid
superOwlParliament_disjointUnionAndCorrect :: OwlTree -> SuperOwlParliament -> SuperOwlParliament -> SuperOwlParliament
superOwlParliament_disjointUnionAndCorrect od (SuperOwlParliament s1) (SuperOwlParliament s2) = r where

  -- first convert s1 into a map
  mapsop0 :: IM.IntMap SuperOwl
  mapsop0 = IM.fromList . toList . fmap (\sowl -> (_superOwl_id sowl, sowl)) $ s1

  addToSelection :: SuperOwl -> IM.IntMap SuperOwl -> IM.IntMap SuperOwl
  addToSelection sowl mapsop = rslt where
    rid = _superOwl_id sowl

    -- add self to map
    rslt' = IM.insert rid sowl mapsop

    -- check if any children are selected and remove them from selection
    children = owliteratechildrenat od rid
    rslt = foldr (\x acc -> IM.delete (_superOwl_id x) acc) rslt' children

  -- assumes sowl is NOT in mapsop and that one of its ancestors is
  -- removes sowl from mapsop and adds its siblings and recurses on its parent until it reaches a selected parent
  removeFromInheritSelection sowl mapsop = rslt where
    prid = superOwl_parentId sowl
    -- the parent is guaranteed to exist because we only call this on elements who inheritSelected
    mommy = owlTree_mustFindSuperOwl od prid
    newkiddos = Seq.deleteAt (_owlItemMeta_position . _superOwl_meta $ sowl) (fromJust $ mommyOwl_kiddos mommy)
    -- add siblings to selection (guaranteed that none of their children are selected by assumption)
    mapsop' = foldr (\rid acc -> IM.insert rid (owlTree_mustFindSuperOwl od rid) acc) mapsop newkiddos
    rslt = if IM.member prid mapsop'
      -- we've reached the selected parent, deselect it and return our new selection
      then IM.delete prid mapsop'
      -- recursively deselect the parent
      else removeFromInheritSelection (owlTree_mustFindSuperOwl od prid) mapsop'

  isDescendentOfOwlMap :: REltId -> IM.IntMap SuperOwl -> Bool
  isDescendentOfOwlMap rid mapsop = if IM.member rid mapsop
    then True
    else case owlTree_findSuperOwl od rid of
      Nothing -> False
      Just x -> isDescendentOfOwlMap (superOwl_parentId x) mapsop

  foldfn sowl acc = if IM.member rid acc
    -- we are selected, remove self from selection
    then IM.delete rid acc
    -- we are not selected
    else if isDescendentOfOwlMap rid acc
      -- parent selected
      then removeFromInheritSelection sowl acc
      -- parent not selected, add self to selection
      else addToSelection sowl acc
    where
      rid = _superOwl_id sowl

  mapsop1 = foldr foldfn mapsop0 s2
  unsortedSeq = Seq.fromList (IM.elems mapsop1)

  r = makeSortedSuperOwlParliament od unsortedSeq

superOwlParliament_isValid :: OwlTree -> SuperOwlParliament -> Bool
superOwlParliament_isValid od sop@(SuperOwlParliament owls) = r
  where
    om = _owlTree_mapping od

    -- check if a mommy owl is selected, that no descendant of that mommy owl is selected
    kiddosFirst = Seq.sortBy (\a b -> flip compare (_owlItemMeta_depth (_superOwl_meta a)) (_owlItemMeta_depth (_superOwl_meta b))) owls
    acc0 = (Set.empty, Set.fromList . toList . fmap _superOwl_id $ owls, True)
    foldlfn (visited, mommies', passing) sowl = (nextVisited, mommies, passMommyCheck && passing)
      where
        -- remove self from list of mommies
        -- TODO you  don't actually need to check two elts at the same level, you can be smarter about removing mommies at each level
        mommies = Set.delete (_superOwl_id sowl) mommies'

        checkMommyRec rid toVisit = case rid of
          -- made it to the top
          x | x == noOwl -> (toVisit, True)
          _ -> case Set.member rid visited of
            -- we've been here before, must be OK
            True -> (toVisit, True)
            False -> case Set.member rid mommies of
              -- one of our mommies, not OK
              True -> (toVisit, False)
              False -> case IM.lookup rid om of
                Nothing -> error $ errorMsg_owlMapping_lookupFail om rid
                -- add self to list of mommies to visit and recurse
                Just (oem, _) -> checkMommyRec (_owlItemMeta_parent oem) (Set.insert rid toVisit)
        (visitedMommies, passMommyCheck) = checkMommyRec (_owlItemMeta_parent (_superOwl_meta sowl)) Set.empty
        nextVisited =
          if passMommyCheck
            then Set.union visited visitedMommies
            else visited

    (_, _, r1) = foldl foldlfn acc0 kiddosFirst

    -- check that parliament is in fact ordered correctly (inefficiently 😭)
    r2 = makeSortedSuperOwlParliament od owls == sop

    r = r1 && r2

superOwlParliament_toSEltTree :: OwlTree -> SuperOwlParliament -> SEltTree
superOwlParliament_toSEltTree od (SuperOwlParliament sowls) = toList $ join r
  where
    makeSElt :: REltId -> SuperOwl -> (REltId, Seq (REltId, SEltLabel))
    makeSElt maxid sowl = case _superOwl_elt sowl of
      OwlItem oinfo (OwlSubItemFolder kiddos) -> (newmaxid,
          Seq.singleton (_superOwl_id sowl, SEltLabel (_owlInfo_name oinfo) SEltFolderStart)
            >< (join childSElts)
            >< Seq.singleton (maxid + 1, SEltLabel (_owlInfo_name oinfo <> "(end)") SEltFolderEnd)
        )
        where
          kiddoS = (unSuperOwlParliament . owlParliament_toSuperOwlParliament od . OwlParliament $ kiddos)
          (newmaxid, childSElts) = mapAccumL makeSElt (maxid + 1) kiddoS
      _ -> (maxid, Seq.singleton $ (_superOwl_id sowl, hasOwlItem_toSEltLabel_hack (_superOwl_elt sowl)))
    (_, r) = mapAccumL makeSElt (owlTree_maxId od) sowls


newtype CanvasSelection = CanvasSelection { unCanvasSelection :: Seq SuperOwl } deriving (Show, Eq)

-- | convert SuperOwlParliament to CanvasSelection (includes children and no folders)
-- does not omits locked/hidden elts since Owl should not depend on Layers, you should do this using filterfn I guess??
superOwlParliament_convertToCanvasSelection :: OwlTree -> (SuperOwl -> Bool) -> SuperOwlParliament -> CanvasSelection
superOwlParliament_convertToCanvasSelection od filterfn (SuperOwlParliament sowls) = r where
  filtered = Seq.filter filterfn sowls
  sopify children = owlParliament_toSuperOwlParliament od (OwlParliament children)
  -- if folder then recursively include children otherwise include self
  mapfn sowl = case _superOwl_elt sowl of
    OwlItem _ (OwlSubItemFolder kiddos) -> unCanvasSelection $ superOwlParliament_convertToCanvasSelection od filterfn (sopify kiddos)
    _ -> Seq.singleton sowl
  r = CanvasSelection . join . fmap mapfn $ filtered

-- converts a SuperOwlParliament to its ordered Seq of SuperOwls including its children
superOwlParliament_convertToSeqWithChildren :: OwlTree -> SuperOwlParliament -> Seq SuperOwl
superOwlParliament_convertToSeqWithChildren od (SuperOwlParliament sowls) = r where
  sopify children = owlParliament_toSuperOwlParliament od (OwlParliament children)
  -- if folder then recursively include children otherwise include self
  mapfn sowl = case _superOwl_elt sowl of
    OwlItem _ (OwlSubItemFolder kiddos) -> sowl <| (superOwlParliament_convertToSeqWithChildren od (sopify kiddos))
    _ -> Seq.singleton sowl
  r = join . fmap mapfn $ sowls

-- | intended for use in OwlWorkspace to create PFCmd
-- generate MiniOwlTree will be reindexed so as not to conflict with OwlTree
-- relies on OwlParliament being correctly ordered
owlParliament_convertToMiniOwltree :: OwlTree -> OwlParliament -> MiniOwlTree
owlParliament_convertToMiniOwltree od op@(OwlParliament owls) = assert valid r where
  valid = superOwlParliament_isValid od $ owlParliament_toSuperOwlParliament od op

  addOwl :: REltId -> REltId -> Seq REltId -> (OwlMapping, IM.IntMap REltId, REltId, SiblingPosition) -> (OwlMapping, IM.IntMap REltId, REltId)
  addOwl newprid rid newchildrids (om, ridremap, nrid, pos) = (newom, newridremap, nrid+1) where
    sowl = owlTree_mustFindSuperOwl od rid
    newoem = OwlItemMeta {
        _owlItemMeta_parent = newprid
        , _owlItemMeta_depth = 0
        , _owlItemMeta_position = pos -- relies on OwlParliament being correctly ordered
      }
    newoe = case _superOwl_elt sowl of
      OwlItem oinfo (OwlSubItemFolder _) -> OwlItem oinfo (OwlSubItemFolder newchildrids)
      x -> x
    newom = IM.insert nrid (newoem, newoe) om
    newridremap = IM.insert rid nrid ridremap

  -- TODO this needs to return remapped rids (use mapAccumL)
  addOwlRecursive :: Int -> REltId -> REltId -> (OwlMapping, IM.IntMap REltId, REltId, SiblingPosition) -> ((OwlMapping, IM.IntMap REltId, REltId, SiblingPosition), REltId)
  addOwlRecursive depth prid rid (om, ridremap, nrid, pos) = rslt where

    newprid = if prid == noOwl then noOwl else ridremap IM.! prid

    -- add self (note that nrid is the new rid of the owl we just added)
    (newom', newridremap', newnrid') = addOwl newprid rid (newchildrids) (om, ridremap, nrid, pos)

    children = fromMaybe Seq.empty $ mommyOwl_kiddos $ owlTree_mustFindSuperOwl od rid

    -- recursively add children
    ((newom, newridremap, newnrid, _), newchildrids) = mapAccumL (\acc crid -> addOwlRecursive (depth+1) rid crid acc) (newom', newridremap', newnrid', 0) children

    rslt = ((newom, newridremap, newnrid, pos+1), nrid)


  -- recursively add all children to owltree and reindex
  ((om1, _, _, _), newtopowls) = mapAccumL (\acc rid -> addOwlRecursive 0 noOwl rid acc) (IM.empty, IM.empty, owlTree_maxId od + 1, 0) owls

  r = OwlTree {
      _owlTree_mapping = om1
      , _owlTree_topOwls = newtopowls
    }


type OwlParliamentSet = IS.IntSet

superOwlParliament_toOwlParliamentSet :: SuperOwlParliament -> OwlParliamentSet
superOwlParliament_toOwlParliamentSet (SuperOwlParliament sowls) = IS.fromList . toList . fmap _superOwl_id $ sowls

owlParliamentSet_member :: REltId -> OwlParliamentSet -> Bool
owlParliamentSet_member = IS.member

-- | returns true if rid is a contained in the OwlParliamentSet or is a descendent of sset
owlParliamentSet_descendent :: OwlTree -> REltId -> OwlParliamentSet -> Bool
owlParliamentSet_descendent ot rid sset = if owlParliamentSet_member rid sset
  then True
  else case owlTree_findSuperOwl ot rid of
    Nothing -> False
    Just x -> owlParliamentSet_descendent ot (superOwl_parentId x) sset

-- UNTESTED
owlParliamentSet_findParents :: OwlTree -> OwlParliamentSet -> OwlParliamentSet
owlParliamentSet_findParents od ops = r where
  foldrfn rid acc = case owlTree_findSuperOwl od rid of
    Nothing -> acc
    Just sowl -> let
        prid = _owlItemMeta_parent (_superOwl_meta sowl)
      in if prid == noOwl
        then acc
        else IS.insert prid acc
  parents = IS.foldr foldrfn IS.empty ops
  superparents = if IS.null parents then IS.empty else owlParliamentSet_findParents od parents
  r = IS.union parents superparents




-- |
data OwlTree = OwlTree
  { _owlTree_mapping :: OwlMapping,
    _owlTree_topOwls :: Seq REltId
  }
  deriving (Show, Eq, Generic)

instance NFData OwlTree

instance MommyOwl OwlTree where
  mommyOwl_kiddos o = Just $ _owlTree_topOwls o

type MiniOwlTree = OwlTree

-- | check if two OwlTree's are equivalent
-- checks if structure is the same, REltIds can differ
owlTree_equivalent :: OwlTree -> OwlTree -> Bool
owlTree_equivalent ota otb = r
  where
    mustFind rid ot = case IM.lookup rid (_owlTree_mapping ot) of
      Nothing -> error $ errorMsg_owlTree_lookupFail ot rid
      Just x -> x

    kiddos_equivalent kiddosa kiddosb =
      Seq.length kiddosa == Seq.length kiddosb
        && all id (Seq.zipWith (owl_equivalent') kiddosa kiddosb)

    owl_equivalent' rida ridb = owl_equivalent a' b'
      where
        (_, a') = mustFind rida ota
        (_, b') = mustFind ridb otb

    owl_equivalent (OwlItem oia (OwlSubItemFolder kiddosa)) (OwlItem oib (OwlSubItemFolder kiddosb)) = oia == oib && kiddos_equivalent kiddosa kiddosb
    owl_equivalent (OwlItem oia osia) (OwlItem oib osib) = oia == oib && owlSubItem_equivalent osia osib

    r = kiddos_equivalent (_owlTree_topOwls ota) (_owlTree_topOwls otb)

instance PotatoShow OwlTree where
  potatoShow od = r where
    foldlfn acc rid =
      let sowl = owlTree_mustFindSuperOwl od rid
          selfEntry' = T.replicate (_owlItemMeta_depth . _superOwl_meta $ sowl) " " <> potatoShow sowl
          selfEntry = selfEntry' <> "\n"
       in acc <> case mommyOwl_kiddos sowl of
            Nothing -> selfEntry
            Just kiddos -> selfEntry <> printKiddos kiddos
    printKiddos :: Seq REltId -> Text
    printKiddos kiddos = foldl foldlfn "" kiddos
    r = printKiddos (fromJust $ mommyOwl_kiddos od)

owlTree_validate :: OwlTree -> (Bool, Text)
owlTree_validate od = checkRecursive "" noOwl 0 (_owlTree_topOwls od)
  where
    checkRecursive msg0 parentrid depth kiddos = r
      where
        foldfn (pass', msg') i rid = case owlTree_findSuperOwl od rid of
          Nothing -> (False, msg' <> "\nmissing REltId " <> show rid)
          Just x -> (rpass, rmsg)
            where
              expected =
                OwlItemMeta
                  { _owlItemMeta_parent = parentrid,
                    _owlItemMeta_depth = depth,
                    _owlItemMeta_position = i
                  }
              rpass1 = pass' && expected == _superOwl_meta x
              rmsg1 = if rpass1 then msg' else msg' <> "\nbad meta at " <> show rid <> " got " <> show (_superOwl_meta x) <> " expected " <> show expected
              (rpass2, rmsg2) = case (mommyOwl_kiddos x) of
                Nothing -> (rpass1, rmsg1)
                Just kiddos' -> checkRecursive msg0 (_superOwl_id x) (depth + 1) kiddos'
              (rpass, rmsg) = (rpass1 && rpass2, rmsg2)
        r = Seq.foldlWithIndex foldfn (True, msg0) kiddos

owlTree_maxId :: OwlTree -> REltId
owlTree_maxId s = maybe 0 fst (IM.lookupMax (_owlTree_mapping s))

-- reorganize the children of the given parent
-- i.e. update their position in the directory
internal_owlTree_reorgKiddos :: OwlTree -> REltId -> OwlTree
internal_owlTree_reorgKiddos od prid = od {_owlTree_mapping = om}
  where
    childrenToUpdate = fromJust $ owlTree_findKiddos od prid
    setRelPos i (oem, oe) = (oem {_owlItemMeta_position = i}, oe)
    om = Seq.foldlWithIndex (\om' i x -> IM.adjust (setRelPos i) x om') (_owlTree_mapping od) childrenToUpdate

emptyOwlTree :: OwlTree
emptyOwlTree =
  OwlTree
    { _owlTree_mapping = IM.empty,
      _owlTree_topOwls = Seq.empty
    }

owlTree_exists :: OwlTree -> REltId -> Bool
owlTree_exists OwlTree {..} rid = IM.member rid _owlTree_mapping

owlTree_findSuperOwl :: OwlTree -> REltId -> Maybe SuperOwl
owlTree_findSuperOwl OwlTree {..} rid = do
  (meta, elt) <- IM.lookup rid _owlTree_mapping
  return $ SuperOwl rid meta elt

owlTree_mustFindSuperOwl :: HasCallStack => OwlTree -> REltId -> SuperOwl
owlTree_mustFindSuperOwl od rid = case owlTree_findSuperOwl od rid of
  Nothing -> error $ errorMsg_owlTree_lookupFail od rid
  Just x -> x

owlTree_findKiddos :: OwlTree -> REltId -> Maybe (Seq REltId)
owlTree_findKiddos OwlTree {..} rid = case rid of
  x | x == noOwl -> return _owlTree_topOwls
  x -> do
    (_, oelt) <- IM.lookup x _owlTree_mapping
    mommyOwl_kiddos oelt

owlTree_findSuperOwlAtOwlSpot :: OwlTree -> OwlSpot -> Maybe SuperOwl
owlTree_findSuperOwlAtOwlSpot od OwlSpot {..} = do
  kiddos <- owlTree_findKiddos od _owlSpot_parent
  kid <- case _owlSpot_leftSibling of
    Nothing -> Seq.lookup 0 kiddos
    -- take until we reach the point and return one to the right
    Just rid -> Seq.lookup 0 . Seq.drop 1 . Seq.dropWhileL (\rid' -> rid' /= rid) $ kiddos
  owlTree_findSuperOwl od kid

-- move one spot to the left, returns Nothing if not possible
owlTree_goRightFromOwlSpot :: OwlTree -> OwlSpot -> Maybe OwlSpot
owlTree_goRightFromOwlSpot od ospot = do
  sowl <- owlTree_findSuperOwlAtOwlSpot od ospot
  return $ ospot {_owlSpot_leftSibling = Just $ _superOwl_id sowl}

-- |
-- throws if OwlItemMeta is invalid in OwlTree
-- TODO make naming consistent in this file...
owlTree_owlItemMeta_toOwlSpot :: OwlTree -> OwlItemMeta -> OwlSpot
owlTree_owlItemMeta_toOwlSpot OwlTree {..} OwlItemMeta {..} = r
  where
    msiblings = case _owlItemMeta_parent of
      x | x == noOwl -> return _owlTree_topOwls
      x -> do
        (_, oelt) <- IM.lookup x _owlTree_mapping
        mommyOwl_kiddos oelt

    siblings = fromJust msiblings
    r =
      OwlSpot
        { _owlSpot_parent = _owlItemMeta_parent,
          _owlSpot_leftSibling = locateLeftSiblingIdFromSiblingPosition _owlTree_mapping siblings _owlItemMeta_position
        }


-- |
-- throws if REltId is invalid in OwlTree
owlTree_rEltId_toOwlSpot :: (HasCallStack) => OwlTree -> REltId -> OwlSpot
owlTree_rEltId_toOwlSpot od@OwlTree {..} rid = r
  where
    (oem, _) = fromJust $ IM.lookup rid _owlTree_mapping
    r = owlTree_owlItemMeta_toOwlSpot od oem

-- |
-- super inefficient implementation for testing only
owlTree_rEltId_toFlattenedIndex_debug :: OwlTree -> REltId -> Int
owlTree_rEltId_toFlattenedIndex_debug od rid = r
  where
    sowls = owliterateall od
    r = fromMaybe (-1) $ Seq.findIndexL (\sowl -> _superOwl_id sowl == rid) sowls

-- |
-- NOTE this will return an AttachmentMap containing targets that have since been deleted
owlTree_makeAttachmentMap :: OwlTree -> AttachmentMap
owlTree_makeAttachmentMap od = attachmentMap_addSuperOwls (owliterateall od) IM.empty

-- | return fales if any attachments are dangling (i.e. they are attached to a target that does not exist in the tree)
owlTree_hasDanglingAttachments :: OwlTree -> Bool
owlTree_hasDanglingAttachments od@OwlTree {..} = not $ all (\sowl -> all (\x -> IM.member x (_owlTree_mapping)) (fmap _attachment_target $ hasOwlItem_attachments sowl)) (owliterateall od)

owlTree_topSuperOwls :: OwlTree -> Seq SuperOwl
owlTree_topSuperOwls od = r
  where
    sowls = fmap (owlTree_mustFindSuperOwl od) (_owlTree_topOwls od)
    areOwlsInFactSuper = all superOwl_isTopOwl sowls
    r = assert areOwlsInFactSuper sowls

owlTree_foldAt' :: (a -> SuperOwl -> a) -> a -> OwlTree -> SuperOwl -> a
owlTree_foldAt' f acc od sowl = case _superOwl_elt sowl of
  OwlItem _ (OwlSubItemFolder kiddos) -> foldl (\acc' rid' -> owlTree_foldAt' f acc' od (owlTree_mustFindSuperOwl od rid')) (f acc sowl) kiddos
  _ -> f acc sowl

-- | fold over an element in the tree and all its children
owlTree_foldAt :: (a -> SuperOwl -> a) -> a -> OwlTree -> REltId -> a
owlTree_foldAt f acc od rid = owlTree_foldAt' f acc od (owlTree_mustFindSuperOwl od rid)

owlTree_foldChildrenAt' :: (a -> SuperOwl -> a) -> a -> OwlTree -> SuperOwl -> a
owlTree_foldChildrenAt' f acc od sowl = case _superOwl_elt sowl of
  OwlItem _ (OwlSubItemFolder kiddos) -> foldl (\acc' rid' -> owlTree_foldAt' f acc' od (owlTree_mustFindSuperOwl od rid')) acc kiddos
  _ -> acc

-- | same as owlTree_foldAt but excludes parent
owlTree_foldChildrenAt :: (a -> SuperOwl -> a) -> a -> OwlTree -> REltId -> a
owlTree_foldChildrenAt f acc od rid = owlTree_foldChildrenAt' f acc od (owlTree_mustFindSuperOwl od rid)

owlTree_fold :: (a -> SuperOwl -> a) -> a -> OwlTree -> a
owlTree_fold f acc0 od = foldl (\acc rid -> owlTree_foldAt f acc od rid) acc0 $ _owlTree_topOwls od

owlTree_owlCount :: OwlTree -> Int
owlTree_owlCount od = owlTree_fold (\acc _ -> acc + 1) 0 od

-- | iterates an element and all its children
owliterateat :: OwlTree -> REltId -> Seq SuperOwl
owliterateat od rid = owlTree_foldAt (|>) Seq.empty od rid where

-- | iterates an element's children (excluding self)
owliteratechildrenat :: OwlTree -> REltId -> Seq SuperOwl
owliteratechildrenat od rid = owlTree_foldChildrenAt (|>) Seq.empty od rid where

-- | iterates everything in the directory
owliterateall :: OwlTree -> Seq SuperOwl
owliterateall od = owlTree_fold (|>) Seq.empty od

class HasOwlTree o where
  hasOwlTree_owlTree :: o -> OwlTree
  hasOwlTree_exists :: o -> REltId -> Bool
  hasOwlTree_exists o rid = hasOwlTree_exists (hasOwlTree_owlTree o) rid
  hasOwlTree_findSuperOwl :: o -> REltId -> Maybe SuperOwl
  hasOwlTree_findSuperOwl o rid = hasOwlTree_findSuperOwl (hasOwlTree_owlTree o) rid
  hasOwlTree_mustFindSuperOwl :: HasCallStack => o -> REltId -> SuperOwl
  hasOwlTree_mustFindSuperOwl o rid = hasOwlTree_mustFindSuperOwl (hasOwlTree_owlTree o) rid

  -- only intended for use in tests
  hasOwlTree_test_findFirstSuperOwlByName :: o -> Text -> Maybe SuperOwl
  hasOwlTree_test_findFirstSuperOwlByName o t = hasOwlTree_test_findFirstSuperOwlByName (hasOwlTree_owlTree o) t
  hasOwlTree_test_mustFindFirstSuperOwlByName :: o -> Text -> SuperOwl
  hasOwlTree_test_mustFindFirstSuperOwlByName o t = fromJust (hasOwlTree_test_findFirstSuperOwlByName o t)

instance HasOwlTree OwlTree where
  hasOwlTree_owlTree = id
  hasOwlTree_exists = owlTree_exists
  hasOwlTree_findSuperOwl = owlTree_findSuperOwl
  hasOwlTree_mustFindSuperOwl = owlTree_mustFindSuperOwl
  hasOwlTree_test_findFirstSuperOwlByName ot label = find (\sowl -> hasOwlItem_name sowl == label) . toList $ owliterateall ot

-- | select everything in the OwlTree
owlTree_toSuperOwlParliament :: OwlTree -> SuperOwlParliament
owlTree_toSuperOwlParliament od@OwlTree {..} = r
  where
    r = owlParliament_toSuperOwlParliament od . OwlParliament $ _owlTree_topOwls

owlTree_removeREltId :: REltId -> OwlTree -> OwlTree
owlTree_removeREltId rid od = owlTree_removeSuperOwl (owlTree_mustFindSuperOwl od rid) od

owlTree_removeSuperOwl :: SuperOwl -> OwlTree -> OwlTree
owlTree_removeSuperOwl sowl OwlTree {..} = r
  where
    -- remove the element itself
    newMapping'' = IM.delete (_superOwl_id sowl) _owlTree_mapping

    -- remove all children recursively
    removeEltWithoutAdjustMommyFn rid mapping = case IM.lookup rid mapping of
      Nothing -> error $ errorMsg_owlMapping_lookupFail mapping rid
      Just (_, OwlItem _ (OwlSubItemFolder kiddos)) -> foldr removeEltWithoutAdjustMommyFn (IM.delete rid mapping) kiddos
      Just _ -> IM.delete rid mapping
    newMapping' = case _superOwl_elt sowl of
      OwlItem _ (OwlSubItemFolder kiddos) -> foldr removeEltWithoutAdjustMommyFn newMapping'' kiddos
      _ -> newMapping''

    removeSuperOwlFromSeq :: Seq REltId -> SuperOwl -> Seq REltId
    removeSuperOwlFromSeq s so = assert (Seq.length s == Seq.length deletedSeq + 1) deletedSeq
      where
        -- sowl meta may be incorrect at this point so we do linear search to remove the elt
        deletedSeq = Seq.deleteAt (fromJust (Seq.elemIndexL (_superOwl_id so) s)) s
        -- TODO switch to this version once you fix issue in owlTree_moveOwlParliament (see comments there)
        --sp = _owlItemMeta_position . _superOwl_meta $ so
        --deletedSeq = Seq.deleteAt sp s

    -- remove from children of the element's mommy if needed
    removeChildFn parent = case parent of
      (oem, OwlItem oinfo (OwlSubItemFolder kiddos)) -> (oem, OwlItem oinfo (OwlSubItemFolder (removeSuperOwlFromSeq kiddos sowl)))
      _ -> error "expected parent to be a folder"
    newMapping = case _owlItemMeta_parent (_superOwl_meta sowl) of
      x | x == noOwl -> newMapping'
      rid -> IM.adjust removeChildFn rid newMapping'

    -- remove from top owls if needed
    newTopOwls =
      if superOwl_isTopOwl sowl
        then removeSuperOwlFromSeq _owlTree_topOwls sowl
        else _owlTree_topOwls

    r' =
      OwlTree
        { _owlTree_mapping = newMapping,
          _owlTree_topOwls = newTopOwls
        }

    r = internal_owlTree_reorgKiddos r' (_owlItemMeta_parent (_superOwl_meta sowl))

owlTree_moveOwlParliament :: OwlParliament -> OwlSpot -> OwlTree -> (OwlTree, [SuperOwl])
owlTree_moveOwlParliament op spot@OwlSpot {..} od@OwlTree {..} = assert isValid r
  where
    sop@(SuperOwlParliament sowls) = owlParliament_toSuperOwlParliament od op

    -- check that we aren't doing circular parenting 😱
    isValid = not $ any (\x -> isDescendentOf _owlTree_mapping x _owlSpot_parent) (fmap _superOwl_id sowls)

    -- NOTE, that _owlItemMeta_position in sowls may be incorrect in the middle of this fold
    -- this forces us to do linear search in the owlTree_removeSuperOwl call rather than use sibling position as index into children D:
    -- TODO fix by always sort from right to left to avoid this
    removedOd = foldl (\acc sowl -> owlTree_removeSuperOwl sowl acc) od sowls

    -- WIP start
    -- ??? I can't remember what this is anymore, did I aready fix this or no? Pretty sure I can just delet all of this
    -- TODO now that we've removed owls, this might invalidate our target position, so we need to reconstruct it
{-
    -- first find the first position to the left (inclusive) of where we our original drop position is that isn't a removed element
    -- ()
    --removed =  sort . fmap (_owlItemMeta_position . _superOwl_owlItemMeta) . filter ((== _owlSpot_parent) . _owlItemMeta_parent . _superOwl_owlItemMeta) $ sowls
    findPos [] pos = pos
    findPos (x:xs) pos = if x == pos
      then go xs (pos-1)
      else pos
    leftSiblingPos = case _owlSpot_leftSibling of
      Nothing -> noOwl
      Just rid -> _owlItemMeta_position . _superOwl_owlItemMeta . owlTree_mustFindSuperOwl od $ rid
    newSpotPos = findPos removed leftSiblingPos

    newSpotLeftSibling = if newSpotPos == noOwl
      then Nothing
      else if _owlSpot_parent == noOwl
        then
        else owlTree_mustFindSuperOwl od _owlSpot_parent
    -}

    -- list of removed element sorted in order
    removed = fmap _superOwl_id
      . sortOn (_owlItemMeta_position . _superOwl_meta)
      . filter ((== _owlSpot_parent) . _owlItemMeta_parent . _superOwl_meta)
      . toList
      $ sowls
    -- list of all siblings on the spot we are dragging to
    origSiblings = fromMaybe (error "expected siblings") $ if _owlSpot_parent == noOwl
      then mommyOwl_kiddos $ od
      else mommyOwl_kiddos $ owlTree_mustFindSuperOwl od _owlSpot_parent
    -- now we will walk from right to left picking out the first elt that is on or after the target spot we are dragging to (_owlSpot_leftSibling) and isn't in the removed list
    findPos ::
      REltId -- ^ original _owlSpot_leftSibling
      -> [REltId] -- ^ list of removed elements
      -> [REltId] -- ^ list of siblings
      -> Bool -- ^ whether we've gone past our target or not
      -> Maybe REltId -- ^ new non-removed leftSibling
    findPos _ _ [] _ = Nothing
    findPos targetrid [] (y:ys) past = if past
      then Just y
      else if y == targetrid
        then Just y
        else findPos targetrid [] ys past
    findPos targetrid (x:xs) (y:ys) past = if past || (y == targetrid)
      then if x == y
        then findPos targetrid xs ys True
        else Just y
      else if x == y
        then findPos targetrid xs ys past
        else findPos targetrid (x:xs) ys past
    newLeftSibling = case _owlSpot_leftSibling of
      Nothing -> Nothing
      Just target -> findPos target (reverse $ toList removed) (reverse $ toList origSiblings) False
    correctedSpot = spot { _owlSpot_leftSibling = newLeftSibling}

    selttree = superOwlParliament_toSEltTree od sop
    r = owlTree_addSEltTree correctedSpot selttree removedOd

-- |
-- assumes SEltTree REltIds do not collide with OwlTree
owlTree_addSEltTree :: OwlSpot -> SEltTree -> OwlTree -> (OwlTree, [SuperOwl])
owlTree_addSEltTree spot selttree od = r where
  -- convert to OwlDirectory
  otherod = owlTree_fromSEltTree selttree
  r = owlTree_addMiniOwlTree spot otherod od

owlTree_reindex :: Int -> OwlTree -> OwlTree
owlTree_reindex start ot = assert valid r where
  valid = owlTree_maxId ot < start
  -- TODO someday, when we're actually worried about id space size (i.e. when we have multi user mode) we will need to do this more efficiently
  adjustkeyfn k = if k == noOwl then noOwl else k + start
  -- adjust keys to their new ones
  oldmap = _owlTree_mapping ot
  newMap' = IM.mapKeysMonotonic adjustkeyfn oldmap
  -- next adjust children and attachments to the new ids
  ridremap = IM.mapWithKey (\rid _ -> adjustkeyfn rid) oldmap
  mapoem oem = oem { _owlItemMeta_parent = adjustkeyfn (_owlItemMeta_parent oem) }
  mapoe oe =
    -- remap attachments
    owlItem_updateAttachments True ridremap
    -- remap kiddos
    $ (case oe of
      OwlItem oinfo (OwlSubItemFolder kiddos) -> OwlItem oinfo (OwlSubItemFolder (fmap adjustkeyfn kiddos))
      x -> x)
  mapowlfn (oem, oe) = (mapoem oem, mapoe oe)
  newMap = fmap mapowlfn newMap'
  newTopOwls = fmap adjustkeyfn (_owlTree_topOwls ot)
  r = OwlTree newMap newTopOwls

-- TODO check that there are no dangling attachments in MiniOwlTree (attach to non existant element), this is expected to be cleaned up in a previous step, use owlTree_hasDanglingAttachments
-- ^ actually this might be OK... or at least we want to check against tree we are attaching to such that if we copy paste something that was attached it keeps those attachments (or maybe we don't!)
owlTree_addMiniOwlTree :: OwlSpot -> MiniOwlTree -> OwlTree -> (OwlTree, [SuperOwl])
owlTree_addMiniOwlTree targetspot miniot od0 = assert (collisions == 0) $ r where
  od1indices = Set.fromList $ IM.keys (_owlTree_mapping od0)
  od2indices = Set.fromList $ IM.keys (_owlTree_mapping miniot)
  collisions = Set.size $ Set.intersection od1indices od2indices

  mapaccumlfn od (spot, sowl) = internal_owlTree_addOwlItem ospot rid oeltmodded od where
    rid = _superOwl_id sowl
    meta = _superOwl_meta sowl
    ospot = if _owlItemMeta_parent meta == noOwl && _owlItemMeta_position meta == 0
      -- first element goes to target spot
      then targetspot
      else if _owlItemMeta_parent meta == noOwl
        -- top level elements share the parent of the target spot
        then spot { _owlSpot_parent = _owlSpot_parent targetspot}
        -- everything else has a valid spot from previous tree
        else spot

    oeltmodded = case _superOwl_elt sowl of
      -- temp remove kiddos from parent as needed by internal_owlTree_addOwlItem
      OwlItem oinfo (OwlSubItemFolder _) -> OwlItem oinfo (OwlSubItemFolder Seq.empty)
      x -> x

  -- go from left to right such that parents/left siblings are added first
  r = mapAccumL mapaccumlfn od0 $ toList $ fmap (\sowl -> (owlTree_owlItemMeta_toOwlSpot miniot (_superOwl_meta sowl), sowl)) (owliterateall miniot)

-- parents NOT allowed :O
internal_owlTree_addOwlItem :: OwlSpot -> REltId -> OwlItem -> OwlTree -> (OwlTree, SuperOwl)
internal_owlTree_addOwlItem OwlSpot {..} rid oitem OwlTree {..} = assert nochildrenifaddingfolder r
  where
    -- if we're adding a folder (in the normal case), ensure it has no children
    nochildrenifaddingfolder = case oitem of
      OwlItem _ (OwlSubItemFolder kiddos) -> Seq.null kiddos
      _ -> True

    -- first add the OwlItem to the mapping
    meta =
      OwlItemMeta
        { _owlItemMeta_parent = _owlSpot_parent,
          _owlItemMeta_depth = case _owlSpot_parent of
            x | x == noOwl -> 0
            _ -> case IM.lookup _owlSpot_parent _owlTree_mapping of
              Nothing -> error $ errorMsg_owlMapping_lookupFail _owlTree_mapping _owlSpot_parent
              Just (x, _) -> _owlItemMeta_depth x + 1,
          -- this will get set correctly when we call internal_owlTree_reorgKiddos later
          _owlItemMeta_position = error "this thunk should never get evaluated"
        }

    newMapping' = IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) rid (meta, oitem) _owlTree_mapping

    -- modify kiddos of the parent we are adding to
    modifyKiddos kiddos = Seq.insertAt position rid kiddos
      where
        position = case _owlSpot_leftSibling of
          Nothing -> 0
          Just leftsibrid -> case Seq.elemIndexL leftsibrid kiddos of
            Nothing -> error $ "expected to find leftmost sibling " <> show leftsibrid <> " in " <> show kiddos
            Just x -> x + 1
    adjustfn (oem, oitem') = case oitem' of
      OwlItem oinfo (OwlSubItemFolder kiddos) -> (oem, OwlItem oinfo (OwlSubItemFolder (modifyKiddos kiddos)))
      _ -> error $ "expected OwlItemFolder"
    newMapping = case _owlSpot_parent of
      x | x == noOwl -> newMapping'
      _ -> assert (IM.member _owlSpot_parent newMapping') $ IM.adjust adjustfn _owlSpot_parent newMapping'
    -- or top owls if there is no parent
    newTopOwls = case _owlSpot_parent of
      x | x == noOwl -> modifyKiddos _owlTree_topOwls
      _ -> _owlTree_topOwls

    r' =
      OwlTree
        { _owlTree_mapping = newMapping,
          _owlTree_topOwls = newTopOwls
        }

    newtree = internal_owlTree_reorgKiddos r' _owlSpot_parent

    newsowl = owlTree_mustFindSuperOwl newtree rid

    r = (newtree, newsowl)

-- OwlItem must not be a parent
owlTree_addOwlItem :: OwlSpot -> REltId -> OwlItem -> OwlTree -> (OwlTree, SuperOwl)
owlTree_addOwlItem = internal_owlTree_addOwlItem

-- this method works for parents IF all children are included in the list and sorted from left to right
owlTree_addOwlItemList :: [(REltId, OwlSpot, OwlItem)] -> OwlTree -> (OwlTree, [SuperOwl])
owlTree_addOwlItemList seltls od0 = r where

  -- TODO test that seltls are valid... (easier said than done)

  mapaccumlfn od (rid,ospot,oitem) = internal_owlTree_addOwlItem ospot rid oitemmodded od where
    osubitemmodded = case _owlItem_subItem oitem of
      -- temp remove kiddos from parent as needed by internal_owlTree_addOwlItem
      OwlSubItemFolder _ -> OwlSubItemFolder Seq.empty
      x -> x
    oitemmodded = OwlItem (_owlItem_info oitem) osubitemmodded

  -- go from left to right such that parents are added first
  (newot, changes) = mapAccumL mapaccumlfn od0 seltls

  r = (newot, changes)


-- TODO TEST
owlTree_superOwl_comparePosition :: OwlTree -> SuperOwl -> SuperOwl -> Ordering
owlTree_superOwl_comparePosition ot sowl1 sowl2 = r where
  m1 = _superOwl_meta sowl1
  m2 = _superOwl_meta sowl2
  d1 = _owlItemMeta_depth m1
  d2 = _owlItemMeta_depth m2
  p1 = _owlItemMeta_parent m1
  p2 = _owlItemMeta_parent m2
  s1 = _owlItemMeta_position m1
  s2 = _owlItemMeta_position m2
  psowl1 = owlTree_mustFindSuperOwl ot p1
  psowl2 = owlTree_mustFindSuperOwl ot p2
  r = if d1 == d2
    then if p1 == p2
      then compare s1 s2
      else owlTree_superOwl_comparePosition ot psowl1 psowl2
    else if d1 > d2
      then owlTree_superOwl_comparePosition ot psowl1 sowl2
      else owlTree_superOwl_comparePosition ot sowl1 psowl2

-- | use to convert old style layers to Owl
internal_addUntilFolderEndRecursive ::
  REltIdMap SEltLabel ->
  Seq REltId ->
  -- | current layer position we are adding
  Int ->
  -- | parent
  REltId ->
  -- | depth
  Int ->
  -- | accumulated directory
  REltIdMap (OwlItemMeta, OwlItem) ->
  -- | accumulated children at current level
  Seq REltId ->
  -- | (next lp, accumulated directory, children of current level)
  (Int, REltIdMap (OwlItemMeta, OwlItem), Seq REltId)
internal_addUntilFolderEndRecursive oldDir oldLayers lp parent depth accDir accSiblings =
  let recurfn = internal_addUntilFolderEndRecursive oldDir oldLayers
      -- the elt we want to add
      rid = Seq.index oldLayers lp
      SEltLabel name selt = oldDir IM.! rid
      selfMeta = OwlItemMeta parent depth (Seq.length accSiblings)
      newSiblings = accSiblings |> rid
   in if lp >= Seq.length oldLayers
        then -- this means we've reached the end of layers, nothing to do
          (lp + 1, accDir, accSiblings)
        else -- normal case
        case selt of
          SEltFolderStart -> r
            where
              (lp', accDir', accSiblings') = recurfn (lp + 1) rid (depth + 1) accDir Seq.empty
              selfOwl = OwlItem (OwlInfo name) (OwlSubItemFolder accSiblings')
              r = recurfn lp' parent depth (IM.insert rid (selfMeta, selfOwl) accDir') newSiblings
          -- we're done! throw out this elt
          SEltFolderEnd -> (lp + 1, accDir, accSiblings)
          -- nothing special, keep going
          _ -> recurfn (lp + 1) parent depth (IM.insert rid (selfMeta, (OwlItem (OwlInfo name) (sElt_to_owlSubItem selt))) accDir) newSiblings

owlTree_fromSEltTree :: SEltTree -> OwlTree
owlTree_fromSEltTree selttree = r
  where
    seltmap = IM.fromList selttree
    layers = fmap fst selttree
    r = owlTree_fromOldState seltmap (Seq.fromList layers)

owlTree_fromOldState :: REltIdMap SEltLabel -> Seq REltId -> OwlTree
owlTree_fromOldState oldDir oldLayers = r
  where
    (_, newDir, topOwls) = internal_addUntilFolderEndRecursive oldDir oldLayers 0 noOwl 0 IM.empty Seq.empty
    r =
      OwlTree
        { _owlTree_mapping = newDir,
          _owlTree_topOwls = topOwls
        }

owlTree_toSEltTree :: OwlTree -> SEltTree
owlTree_toSEltTree od = superOwlParliament_toSEltTree od (owlTree_toSuperOwlParliament od)

-- DELETE use hasOwlElt variant
superOwl_toSElt_hack :: SuperOwl -> SElt
superOwl_toSElt_hack = hasOwlItem_toSElt_hack . _superOwl_elt

-- DELETE use hasOwlElt variant
superOwl_toSEltLabel_hack :: SuperOwl -> SEltLabel
superOwl_toSEltLabel_hack = hasOwlItem_toSEltLabel_hack . _superOwl_elt
