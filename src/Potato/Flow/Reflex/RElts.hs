{-# LANGUAGE RecursiveDo #-}
module Potato.Flow.Reflex.RElts (
  REltId
  , RElt(..)
  , REltLabel(..)
  , REltLabelWithId
  , REltTree(..)
  , serialize
  , deserialize
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Layers
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Monad.Fix

import           Reflex

-- TODO move to reltfactory
type REltId = Int

-- TODO node names
data RElt t = REltNone | REltFolderStart | REltFolderEnd | REltBox (Dynamic t SBox) | REltLine (Dynamic t SLine) | REltText (Dynamic t SText)


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
  re_name     :: Text
  , re_elt    :: RElt t
  , re_reflex :: REltReflex t
}

type REltLabelWithId t = (REltId, REltLabel t)

instance LayerElt (REltLabelWithId t) where
  type LayerEltId (REltLabelWithId t) = REltId
  isFolderStart (_,rel) = case re_elt rel of
    REltFolderStart -> True
    _               -> False
  isFolderEnd (_,rel) = case re_elt rel of
    REltFolderEnd -> True
    _             -> False
  getId = fst


-- expected to satisfy scoping invariant
type REltTree t = [REltLabel t]


deserialize :: (Reflex t, MonadHold t m, MonadFix m) => SEltTree -> m (REltTree t)
deserialize [] = return []
deserialize (SEltLabel sname selt:rest) = mdo


  -- TODO implement for each type
  (relt, rreflex) <- case selt of
    SEltNone        -> return (REltNone, nilReflex)
    SEltFolderStart -> return (REltFolderStart, nilReflex)
    SEltFolderEnd   -> return (REltFolderEnd, nilReflex)
    _               -> undefined
    --SEltBox x -> hold

  -- TODO generate node id

  reltRest <- deserialize rest
  return $ (REltLabel sname relt rreflex) : reltRest


serialize :: (Reflex t, MonadSample t m) => REltTree t -> m SEltTree
serialize [] = return []
serialize (relt:rest) = do
  let
    sampleDyn = sample . current
  selt <- case re_elt relt of
    REltNone        -> return SEltNone
    REltFolderStart -> return SEltFolderStart
    REltFolderEnd   -> return SEltFolderEnd
    REltBox x       -> SEltBox <$> sampleDyn x
    REltLine x      -> SEltLine <$> sampleDyn x
    REltText x      -> SEltText <$> sampleDyn x
  seltRest <- serialize rest
  return $ (SEltLabel (re_name relt) selt) : seltRest
