{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Owl where

import           Relude

import           Potato.Flow.Types
import           Potato.Flow.SElts

import           Control.Exception (assert)
import qualified Data.IntMap       as IM
import           Data.Sequence     ((|>))
import qualified Data.Sequence     as Seq
import Data.Foldable (foldl)
import Data.Maybe (fromJust)
import qualified Data.Text as T


-- TODO Consider moving OwlInfo into meta?
data OwlInfo = OwlInfo { _owlInfo_name :: Text } deriving (Show, Generic)

data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)

-- TODO no owl prefix prob
type SemiPos = Int

topOwlParent :: REltId
topOwlParent = -1

data OwlEltMeta = OwlEltMeta {
  _owlEltMeta_parent :: REltId
  , _owlEltMeta_depth :: Int
  , _owlEltMeta_relPosition :: SemiPos
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
superOwl_isTopOwlSurely SuperOwl {..}  = _owlEltMeta_depth _superOwl_meta == 0 &&_owlEltMeta_parent _superOwl_meta == topOwlParent

data OwlDirectory = OwlDirectory {
  _owlDirectory_directory :: REltIdMap (OwlEltMeta, OwlElt)
  , _owlDirectory_topOwls :: Seq REltId
} deriving (Show)

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
-- inlining... hope this will tell me where caller came from when fromJust fails
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

-- TODO need rel position to insert at I guess
owlDirectory_addSuperOwl :: OwlDirectory -> SuperOwl -> OwlDirectory
owlDirectory_addSuperOwl OwlDirectory{..} sowl@SuperOwl {..} = assert (superOwl_isTopOwl sowl) r where
  newDirectory = IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) _superOwl_id (_superOwl_meta, _superOwl_elt) _owlDirectory_directory
  --newDirectoryNoAssert = IM.insert _superOwl_id (_superOwl_meta, _superOwl_elt) _owlDirectory_directory
  r = OwlDirectory {
      _owlDirectory_directory = newDirectory
      -- TODO insert at provided position
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
  (_, newDir, topOwls) = addUntilFolderEndRecursive oldDir oldLayers 0 topOwlParent 0 IM.empty Seq.empty
  r = OwlDirectory {
      _owlDirectory_directory = newDir
      , _owlDirectory_topOwls = topOwls
    }


--type SuperOwl = (REltId, OwlEltMeta, OwlElt)

--type OwlDirectory = REltIdMap (OwlEltMeta, OwlElt)

--emptyDirectory :: OwlDirectory
--emptyDirectory = IM.singleton (topOwlId, OwlEltFolder (OwlInfo "top owl") Seq.empty)


-- | get the top owl
-- every OwlDirectory is expected to have a topOwl, fails if no topOwl
--topOwl :: OwlDirectory ->
-- top owl
-- you can either have dummy REltId 0 node in tree OR you can carry around a list of all top level owls with the directory
-- prob way easier to do it the first way...


toSElt :: OwlElt -> SElt
toSElt = undefined

--getChildren :: OwlElt -> Seq
