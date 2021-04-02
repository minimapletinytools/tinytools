{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Owl where

import           Relude

import           Potato.Flow.Types
import           Potato.Flow.SElts

import           Control.Exception (assert)
import qualified Data.IntMap       as IM
import           Data.Sequence     ((|>), (><))
import qualified Data.Sequence     as Seq
import Data.Foldable (foldl)
import Data.Maybe (fromJust)
import qualified Data.Text as T


-- TODO Consider moving OwlInfo into meta?
data OwlInfo = OwlInfo { _owlInfo_name :: Text } deriving (Show, Generic)

data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)

type OwlMapping = REltIdMap (OwlEltMeta, OwlElt)

owlElt_name :: OwlElt -> Text
owlElt_name (OwlEltFolder (OwlInfo name) _) = name
owlElt_name (OwlEltSElt (OwlInfo name) _) = name

-- TODO decide if we want some relative position index or if we just want to use true index (and recompute on move/add/delete)
type SemiPos = Int

-- TODO change this to do a binary search (once you have decided SemiPos is what you want and not actual position)
locateFromSemiPos :: (a -> SemiPos) -> Seq a -> SemiPos -> Int
locateFromSemiPos f s sp = Seq.length $ Seq.takeWhileL (\a -> f a < sp) s

owlMappingSemiPosLookup :: OwlMapping -> REltId -> SemiPos
owlMappingSemiPosLookup om rid = case IM.lookup rid om of
  Nothing -> error $ "expected to find rid " <> show rid
  Just (oem,_) -> _owlEltMeta_relPosition oem

locateOwlFromSemiPos :: OwlMapping -> Seq REltId -> SemiPos -> Int
locateOwlFromSemiPos om s sp = locateFromSemiPos (owlMappingSemiPosLookup om) s sp

-- in this case, we remove only if there is an exact match
removeAtSemiPos :: (a -> SemiPos) -> Seq a -> SemiPos -> Seq a
removeAtSemiPos f s sp = r where
  (front, back) = Seq.breakl (\a -> f a == sp) s
  r = front >< Seq.drop 1 back

removeSuperOwlFromSeq :: OwlMapping -> Seq REltId -> SuperOwl -> Seq REltId
removeSuperOwlFromSeq om s so = assert (Seq.length s == Seq.length r + 1) r where
  sp = _owlEltMeta_relPosition . _superOwl_meta $ so
  r = removeAtSemiPos (owlMappingSemiPosLookup om) s sp


data OwlEltMeta = OwlEltMeta {
  _owlEltMeta_parent :: REltId -- or should we do Maybe REltId?
  , _owlEltMeta_depth :: Int
  , _owlEltMeta_relPosition :: SemiPos
} deriving (Show, Generic)

-- a simpler version of OwlEltMeta used for inserting new Owls
data OwlSpot = OwlSpot {
  _owlSpot_parent :: REltId
  -- TODO is this what we want?
  , _owlSpot_leftSibling :: REltId
} deriving (Show, Generic)

data SuperOwl = SuperOwl {
  _superOwl_id :: REltId
  , _superOwl_meta :: OwlEltMeta
  , _superOwl_elt :: OwlElt
} deriving (Show, Generic)

--superOwl_id :: Lens' SuperOwl REltId
superOwl_id :: Functor f => (REltId -> f REltId) -> SuperOwl -> f SuperOwl
superOwl_id f sowl = fmap (\rid -> sowl { _superOwl_id = rid }) (f (_superOwl_id sowl))

-- TODO rest of lenses

superOwl_isTopOwl :: SuperOwl -> Bool
superOwl_isTopOwl SuperOwl {..} = _owlEltMeta_depth _superOwl_meta == 0

-- | same as superOwl_isTopOwl except checks all conditions, intended to be used in asserts
superOwl_isTopOwlSurely :: SuperOwl -> Bool
superOwl_isTopOwlSurely SuperOwl {..}  = _owlEltMeta_depth _superOwl_meta == 0 &&_owlEltMeta_parent _superOwl_meta == noOwl

noOwl :: REltId
noOwl = -1

-- if parent is selected, then so are all its kiddos
newtype OwlParliament = OwlParliament { unOwlParliament :: Seq REltId }
newtype SuperOwlParliament = SuperOwlParliament { unSuperOwlParliament :: Seq SuperOwl }

-- TODO
owlSuperParliament_isValid :: SuperOwlParliament -> Bool
owlSuperParliament_isValid (SuperOwlParliament owls) = undefined

-- ???
--makeSuperOwlParliament :: [REltId] -> SuperOwlParliament

-- TODO rename to OwlTree
data OwlDirectory = OwlDirectory {
  -- TODO rename to mapping
  _owlDirectory_directory :: OwlMapping
  , _owlDirectory_topOwls :: Seq REltId
} deriving (Show)

-- reorganize the children of the given parent
-- i.e. update their relPosition in the directory
reorgChildren :: OwlDirectory -> REltId -> OwlDirectory
reorgChildren od prid = od { _owlDirectory_directory = om } where
  childrenToUpdate = case prid of
    -1 -> _owlDirectory_topOwls od
    _ -> case IM.lookup prid (_owlDirectory_directory od) of
      Just (_, OwlEltFolder _ children) -> children
      Just _ -> Seq.empty
      Nothing -> error $ "expected to find parent with REltId " <> show prid
  setRelPos i (oem, oe) = (oem { _owlEltMeta_relPosition = i }, oe)
  om = Seq.foldlWithIndex (\om' i x -> IM.adjust (setRelPos i) x om') (_owlDirectory_directory od) childrenToUpdate


emptyDirectory :: OwlDirectory
emptyDirectory = OwlDirectory {
    _owlDirectory_directory = IM.empty
    , _owlDirectory_topOwls = Seq.empty
  }

owlDirectory_findSuperOwl :: REltId -> OwlDirectory -> Maybe SuperOwl
owlDirectory_findSuperOwl rid OwlDirectory {..} = do
  (meta, elt) <- IM.lookup rid _owlDirectory_directory
  return $ SuperOwl rid meta elt

owlDirectory_mustFindSuperOwl :: REltId -> OwlDirectory -> SuperOwl
owlDirectory_mustFindSuperOwl rid od = fromJust $ owlDirectory_findSuperOwl rid od
-- inlining... hope this will tell me where caller came from when fromJust fails.. I don't think it works...
{-# INLINE owlDirectory_mustFindSuperOwl #-}

owlDirectory_topSuperOwls :: OwlDirectory -> Seq SuperOwl
owlDirectory_topSuperOwls od = r where
  sowls = fmap (flip owlDirectory_mustFindSuperOwl od) (_owlDirectory_topOwls od)
  areOwlsInFactSuper = all superOwl_isTopOwl sowls
  r = assert areOwlsInFactSuper sowls

owlDirectory_foldAt' :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> SuperOwl -> a
owlDirectory_foldAt' f acc od sowl = case _superOwl_elt sowl of
  OwlEltFolder _ children -> foldl (\acc' rid' -> owlDirectory_foldAt' f acc' od (owlDirectory_mustFindSuperOwl rid' od)) (f acc sowl) children
  _ -> f acc sowl

owlDirectory_foldAt :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> REltId -> a
owlDirectory_foldAt f acc od rid = owlDirectory_foldAt' f acc od (owlDirectory_mustFindSuperOwl rid od)

owlDirectory_fold :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> a
owlDirectory_fold f acc0 od = foldl (\acc rid -> owlDirectory_foldAt f acc od rid) acc0 $ _owlDirectory_topOwls od

owlDirectory_owlCount :: OwlDirectory -> Int
owlDirectory_owlCount od = owlDirectory_fold (\acc _ -> acc+1) 0 od

owlDirectory_prettyPrint :: OwlDirectory -> [Text]
owlDirectory_prettyPrint od = reverse $ owlDirectory_fold f [] od where
  f acc (SuperOwl rid OwlEltMeta {..} oelt) = r:acc where
    -- TODO make helper for this
    name = case oelt of
      OwlEltFolder (OwlInfo name) _ -> name
      OwlEltSElt (OwlInfo name) _ -> name
    depth = _owlEltMeta_depth
    r = T.replicate depth "  " <> show rid <> " " <> name


-- | iterates an element and all its children
owliterateat :: OwlDirectory -> REltId -> Seq SuperOwl
owliterateat od rid = owlDirectory_foldAt (|>) Seq.empty od rid where

-- | iterates everything in the directory
owliterateall :: OwlDirectory -> Seq SuperOwl
owliterateall od = owlDirectory_fold (|>) Seq.empty od

owlDirectory_removeSuperOwl :: SuperOwl -> OwlDirectory -> OwlDirectory
owlDirectory_removeSuperOwl sowl@SuperOwl{..} od@OwlDirectory{..} = r where
  -- TODO finish
  newDirectory = undefined
  newTopOwls = if superOwl_isTopOwl sowl
    then _owlDirectory_topOwls -- TODO
    else _owlDirectory_topOwls -- TODO we also need to remove from parent
  r = OwlDirectory {
      _owlDirectory_directory = newDirectory
      , _owlDirectory_topOwls = newTopOwls
    }

-- TODO
owlDirectory_moveSuperOwl :: SuperOwl -> OwlSpot -> OwlDirectory -> OwlDirectory
owlDirectory_moveSuperOwl = undefined

-- TODO need rel position to insert at I guess
-- TODO SuperOwl probably the wrong type
owlDirectory_addSuperOwl :: OwlSpot -> SuperOwl -> OwlDirectory -> OwlDirectory
owlDirectory_addSuperOwl OwlSpot{..} sowl@SuperOwl {..} OwlDirectory{..} = assert (superOwl_isTopOwl sowl) r where
  newDirectory = IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) _superOwl_id (_superOwl_meta, _superOwl_elt) _owlDirectory_directory
  --newDirectoryNoAssert = IM.insert _superOwl_id (_superOwl_meta, _superOwl_elt) _owlDirectory_directory
  r = OwlDirectory {
      _owlDirectory_directory = newDirectory
      -- TODO insert at provided position
      -- if sibling is Nothing then insert at 0 position otherwise find the sibling owl
      -- then figure out sibling owl index (binary search on rel pos) OR just use actual position you know..
      , _owlDirectory_topOwls = _owlDirectory_topOwls |> _superOwl_id
    }

-- | use to convert old style layers to Owl
addUntilFolderEndRecursive ::
  REltIdMap SEltLabel
  -> Seq REltId
  -> LayerPos -- ^ current layer position we are adding
  -> REltId -- ^ parent
  -> Int -- ^ depth
  -> REltIdMap (OwlEltMeta, OwlElt) -- ^ accumulated directory
  -> Seq REltId -- ^ accumulated children at current level
  -> (LayerPos, REltIdMap (OwlEltMeta, OwlElt), Seq REltId) -- ^ (next lp, accumulated directory, children of current level)
addUntilFolderEndRecursive oldDir oldLayers lp parent depth accDir accSiblings = let
    recurfn = addUntilFolderEndRecursive oldDir oldLayers
    -- the elt we want to add
    rid = Seq.index oldLayers lp
    SEltLabel name selt =  oldDir IM.! rid
    selfMeta = OwlEltMeta parent depth (Seq.length accSiblings)
    newSiblings = accSiblings |> rid
  in if lp >= Seq.length oldLayers
    -- this means we've reached the end of layers, nothing to do
    then (lp+1, accDir, accSiblings)
    -- normal case
    else case selt of
      SEltFolderStart -> r where
          (lp', accDir', accSiblings') = recurfn (lp+1) rid (depth+1) accDir Seq.empty
          selfOwl = OwlEltFolder (OwlInfo name) accSiblings'
          r = recurfn lp' parent depth (IM.insert rid (selfMeta, selfOwl) accDir') newSiblings
      -- we're done! throw out this elt
      SEltFolderEnd -> (lp+1, accDir, accSiblings)
      -- nothing special, keep going
      _ -> recurfn (lp+1) parent depth (IM.insert rid (selfMeta, OwlEltSElt (OwlInfo name) selt) accDir) newSiblings


owlDirectory_fromOldState :: REltIdMap SEltLabel -> Seq REltId -> OwlDirectory
owlDirectory_fromOldState oldDir oldLayers = r where
  (_, newDir, topOwls) = addUntilFolderEndRecursive oldDir oldLayers 0 noOwl 0 IM.empty Seq.empty
  r = OwlDirectory {
      _owlDirectory_directory = newDir
      , _owlDirectory_topOwls = topOwls
    }
