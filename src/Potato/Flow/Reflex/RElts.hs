{-# LANGUAGE RecursiveDo #-}
module Potato.Flow.Reflex.RElts (
  REltTree(..)
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Monad.Fix

import           Reflex



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

-- TODO sheould contain hot node id (not serialized)
-- | reflex element nodes
data REltLabel t = REltLabel {
  re_name     :: Text
  , re_elt    :: RElt t
  , re_reflex :: REltReflex t
}


type REltTree t = [REltLabel t]

-- TODO
type REltNodeRef t = ()


-- TODO finish
-- | events related to changing topology of REltTree
data REltTopologyEvents t = REltTopologyEvents {

  -- this is a little weird, we only want to trigger this event when we want to GC
  -- e.g. if we are pruning old elements in our action stack
  -- only need this if we are using map approach and not zipper approach
  trash         :: Event t () -- event to gc self and children (not responsible for parent)

  , deleteChild :: Event t (REltNodeRef t) -- event to delete child of this node

  -- TODO signature needs work
  , addChild    :: Event t (REltNodeRef t, Int) -- event to add a child to this node
}

{-
-actions
  -each manipulator is connected to elt params and has events to update it
    -e.g. boxEvent :: SBoxManipulator -> (Event t LPoint, Event t LSize)
  -front-end connects to behavior and events
    -e.g. elt behaviors => front-end =interaction=> manipulator events => elt behaviors
  -SBoxManipulator will create events of the following type:
    Event t (Action )
  -undo/redo :: Event t () -> Dynamic (Stack Action) -> Event Action
-}

-- TODO remove parent as it breaks our fmap
-- TODO need to pass in add/remove child events throughtout the tree
-- TODO need to pass in elt update events throughout the tree
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
