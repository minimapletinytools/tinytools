{-# LANGUAGE RecursiveDo #-}
module Potato.Flow.Reflex.RElts (
  REltId
  , ManipulatorWithId
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


-- reflex vars for an RElt
data REltReflex t = REltReflex {
  -- Behaviors
  re_raycast :: Behavior t LRaycast
  , re_draw  :: Behavior t Renderer -- switch to [Renderer] for better performance
}

nilReflex :: (Reflex t) => REltReflex t
nilReflex = REltReflex {
    re_raycast = constant (const False)
    , re_draw = constant (Renderer (LBox (LPoint zeroXY) (LSize zeroXY)) (const Nothing))
  }

-- | reflex element nodes
data REltLabel t = REltLabel {
  re_id       :: REltId
  , re_name   :: Text
  , re_elt    :: RElt t
  , re_reflex :: REltReflex t
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
  => Event t (ManipulatorWithId t) -- ^ selected do action
  -> Event t (ManipulatorWithId t) -- ^ selected undo action
  -> SEltLabelWithId
  -> m (REltLabel t)
deserializeRElt doev undoev (reltid, SEltLabel sname selt) = do
  -- TODO implement for each type
  (relt, rreflex) <- case selt of
    SEltNone        -> return (REltNone, nilReflex)
    SEltFolderStart -> return (REltFolderStart, nilReflex)
    SEltFolderEnd   -> return (REltFolderEnd, nilReflex)
    _               -> undefined
    --SEltBox x -> hold
  return $ REltLabel reltid sname relt rreflex
deserialize ::
  (Reflex t, MonadHold t m, MonadFix m)
  => Event t (ManipulatorWithId t) -- ^ selected do action
  -> Event t (ManipulatorWithId t) -- ^ selected undo action
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
