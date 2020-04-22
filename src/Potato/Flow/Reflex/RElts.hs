{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Potato.Flow.Reflex.RElts (
  REltId
  , ManipulatorWithId
  , ControllerWithId
  , RElt(..)
  , REltLabel(..)
  , REltTree
  , NonEmptyREltTree
  , SEltLabelWithId
  , SEltWithIdTree
  , serialize
  , deserialize
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Layers
import           Potato.Flow.Reflex.Manipulators
import           Potato.Flow.SElts

import           Control.Monad.Fix

import           Data.Dependent.Sum              (DSum ((:=>)), (==>))
import qualified Data.Dependent.Sum              as DS
import           Data.Functor.Misc
import           Data.Maybe                      (fromJust)

import           Reflex
import           Reflex.Potato.Helpers

-- TODO move to reltfactory
type REltId = Int

type ManipulatorWithId t = DS.DSum (Const2 REltId (Manipulators t)) Identity
-- TODO change to ControllersWithId = DS.DMap
type ControllerWithId = DS.DSum (Const2 REltId Controllers) Identity
type ControllerEventSelector t = EventSelector t (Const2 REltId Controllers)

data RElt t =
  REltNone
  | REltFolderStart
  | REltFolderEnd
  | REltBox (MBox t)
  | REltLine (MLine t)
  | REltText (MText t)

getREltManipulator :: RElt t -> Manipulators t
getREltManipulator relt = case relt of
  REltNone        -> none
  REltFolderStart -> none
  REltFolderEnd   -> none
  REltBox x       -> MTagBox ==> x
  REltLine x      -> MTagLine ==> x
  REltText x      -> MTagText ==> x
  where
    none = MTagNone ==> ()

-- | gets an 'LBox' that contains the entire RElt
getREltBox :: (Reflex t) => RElt t -> Maybe (Dynamic t LBox)
getREltBox relt = case relt of
  REltNone        -> Nothing
  REltFolderStart -> Nothing
  REltFolderEnd   -> Nothing
  REltBox x       -> Just $ _mBox_box x
  REltLine x      -> Just
    $ uncurry make_LBox_from_LPoints
    <$> ffor2 (_mLine_start x) (_mLine_end x) (,)
  REltText x      -> Just $ _mText_box x

data Renderer = Renderer LBox (LPoint -> Maybe PChar)

makePotatoRenderer :: LBox -> Renderer
makePotatoRenderer lbox = Renderer lbox $ \p -> if does_LBox_contains_LPoint lbox p
  then Just '#'
  else Nothing

data REltDrawer t = REltDrawer {
  _rEltDrawer_box        :: Behavior t LBox
  , _rEltDrawer_renderer :: Behavior t Renderer -- switch to [Renderer] for better performance
}

nilDrawer :: (Reflex t) => REltDrawer t
nilDrawer = REltDrawer {
    _rEltDrawer_box = constant nilLBox
    , _rEltDrawer_renderer = constant (Renderer nilLBox (const Nothing))
  }

getDrawer :: (Reflex t) => RElt t -> REltDrawer t
getDrawer relt = case relt of
  REltNone        -> nilDrawer
  REltFolderStart -> nilDrawer
  REltFolderEnd   -> nilDrawer
  REltBox x       -> potatoDrawer
  REltLine x      -> potatoDrawer
  REltText x      -> potatoDrawer
  where
    potatoDrawer = REltDrawer {
        _rEltDrawer_box = current $ fromJust (getREltBox relt)
        , _rEltDrawer_renderer = current $ makePotatoRenderer <$> fromJust (getREltBox relt)
      }

-- | reflex element nodes
data REltLabel t = REltLabel {
  re_id     :: REltId
  , re_name :: Text
  , re_elt  :: RElt t
}

instance LayerElt (REltLabel t) where
  type LayerEltId (REltLabel t) = REltId
  isFolderStart rel = case re_elt rel of
    REltFolderStart -> True
    _               -> False
  isFolderEnd rel = case re_elt rel of
    REltFolderEnd -> True
    _             -> False
  getId = re_id

-- expected to satisfy scoping invariant
type REltTree t = [REltLabel t]
type NonEmptyREltTree t = NonEmpty (REltLabel t)

-- IDs must be assigned first before we can deserialize
type SEltLabelWithId = (REltId, SEltLabel)
type SEltWithIdTree = [SEltLabelWithId]

deserializeRElt ::
  forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => ControllerEventSelector t -- ^ event selector for do action
  -> ControllerEventSelector t -- ^ event selector for undo action
  -> SEltLabelWithId
  -> m (REltLabel t)
deserializeRElt doSelector undoSelector (reltid, SEltLabel sname selt) = do

  let
    reltDoEv = select doSelector (Const2 reltid)
    reltUndoEv = select undoSelector (Const2 reltid)
    bothEv :: Event t (Either Controllers Controllers)
    bothEv = alignEitherWarn ("("<>show reltid<>") do/undo") reltDoEv reltUndoEv

  -- TODO implement for each type
  relt <- case selt of
    SEltNone        -> return REltNone
    SEltFolderStart -> return REltFolderStart
    SEltFolderEnd   -> return REltFolderEnd
    SEltBox SBox {..} -> do
      let
        foldfn :: Either Controllers Controllers -> LBox -> LBox
        foldfn (Left ct) box = case ct of
          (CTagBox :=> Identity dbox) -> plusDelta box dbox
        foldfn (Right ct) box = case ct of
          (CTagBox :=> Identity dbox) -> minusDelta box dbox
      mbox <- foldDyn foldfn sb_box bothEv
      return $ REltBox (MBox mbox)

{-
    -- TODO wut a pain DDDDD:
    SEltLine SLine {..} -> do
      let
        foldfn :: Either Controllers Controllers -> LPoint -> LPoint
        foldfn
      --sl_start   :: LPoint
      --, sl_end  ::LPoint
      mline <- MLine startl endl
-}

    _               -> undefined
  return $ REltLabel reltid sname relt

deserialize ::
  (Reflex t, MonadHold t m, MonadFix m)
  => ControllerEventSelector t -- ^ event selector for do action
  -> ControllerEventSelector t -- ^ event selector for undo action
  -> SEltWithIdTree
  -> m (REltTree t)
deserialize doSelector undoSelector = mapM (deserializeRElt doSelector undoSelector)

serializeRElt :: (Reflex t, MonadSample t m) => REltLabel t -> m SEltLabel
serializeRElt relt = do
  --let
    --sampleDyn = sample . current
  selt <- case re_elt relt of
    REltNone        -> return SEltNone
    REltFolderStart -> return SEltFolderStart
    REltFolderEnd   -> return SEltFolderEnd
    REltBox x       -> undefined --SEltBox <$> sampleDyn x
    REltLine x      -> undefined --SEltLine <$> sampleDyn x
    REltText x      -> undefined --SEltText <$> sampleDyn x
  return $ SEltLabel (re_name relt) selt
serialize :: (Reflex t, MonadSample t m) => REltTree t -> m SEltTree
serialize = mapM serializeRElt
