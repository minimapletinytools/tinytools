{-# LANGUAGE RecursiveDo #-}
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
import           Potato.Flow.Types

import           Control.Monad.Fix

import           Data.Dependent.Sum              (DSum ((:=>)), (==>))
import qualified Data.Dependent.Sum              as DS
import           Data.Functor.Misc

import           Reflex

-- TODO move to reltfactory
type REltId = Int

type ManipulatorWithId t = DS.DSum (Const2 REltId (Manipulators t)) Identity
type ControllerWithId = DS.DSum (Const2 REltId Controllers) Identity

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
    $ fmap (\l -> make_LBox_from_LPoints (fst l) (snd l))
    $ ffor2 (_mLine_start x) (_mLine_end x) (,)
  REltText x      -> Just $ _mText_box x

-- TODO rename this
data REltDrawer t = REltDrawer {
  -- Behaviors
  re_raycast :: Behavior t LRaycast
  , re_draw  :: Behavior t Renderer -- switch to [Renderer] for better performance
}

nilDrawer :: (Reflex t) => REltDrawer t
nilDrawer = REltDrawer {
    re_raycast = constant (const False)
    , re_draw = constant (Renderer (LBox (LPoint zeroXY) (LSize zeroXY)) (const Nothing))
  }

-- TODO finish
getDrawer :: (Reflex t) => RElt t -> REltDrawer t
getDrawer relt = case relt of
  REltNone        -> nilDrawer
  REltFolderStart -> nilDrawer
  REltFolderEnd   -> nilDrawer
  REltBox x       -> nilDrawer
  REltLine x      -> nilDrawer
  REltText x      -> nilDrawer

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
  (Reflex t, MonadHold t m, MonadFix m)
  => Event t (ControllerWithId) -- ^ selected do action
  -> Event t (ControllerWithId) -- ^ selected undo action
  -> SEltLabelWithId
  -> m (REltLabel t)
deserializeRElt doev undoev (reltid, SEltLabel sname selt) = do
  -- TODO implement for each type
  relt <- case selt of
    SEltNone        -> return REltNone
    SEltFolderStart -> return REltFolderStart
    SEltFolderEnd   -> return REltFolderEnd
    _               -> undefined
    --SEltBox x -> hold
  return $ REltLabel reltid sname relt

deserialize ::
  (Reflex t, MonadHold t m, MonadFix m)
  => Event t (ControllerWithId) -- ^ selected do action
  -> Event t (ControllerWithId) -- ^ selected undo action
  -> SEltWithIdTree
  -> m (REltTree t)
deserialize doev undoev = mapM (deserializeRElt doev undoev)

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
