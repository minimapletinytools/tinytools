module Potato.Flow.Reflex.RElts (

) where

import           Relude

import           Potato.Flow.Math
import qualified Potato.Flow.Reflex.Tree as RT
import           Potato.Flow.SElts
import           Potato.Flow.Types

import qualified Data.Tree               as T

import           Reflex

data RElt t = REltNone | REltBox (Dynamic t SBox) | REltLine (Dynamic t SLine) | REltText (Dynamic t SText)

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
  re_elt      :: RElt t
  , re_reflex :: REltReflex t
}

type REltTree t = RT.Tree t (REltLabel t)



-- TODO need to pass in add/remove child events throughtout the tree
-- TODO need to pass in elt update events throughout the tree
deserialize :: (Reflex t, MonadHold t m) => SEltTree -> m (REltTree t)
deserialize (T.Node selt children) = do


  -- TODO implement for each type
  (relt, rreflex) <- case selt of
    SEltNone -> return (REltNone, nilReflex)
    _        -> undefined
    --SEltBox x -> hold

  -- TODO
  treeUpdate <- undefined
  rchildren' <- mapM deserialize children
  rchildren <- holdDyn rchildren' treeUpdate
  let
    label =
      REltLabel {
        re_elt = relt
        , re_reflex = rreflex
      }
    node = RT.Node label rchildren
  return node


serialize :: (Reflex t, MonadSample t m) => REltTree t -> m SEltTree
serialize rnode = do
  let
    sampleDyn = sample . current
  rslabel <- case (re_elt . RT.rootLabel) rnode of
    REltNone   -> return SEltNone
    REltBox x  -> SEltBox <$> sampleDyn x
    REltLine x -> SEltLine <$> sampleDyn x
    REltText x -> SEltText <$> sampleDyn x
  children' :: [REltTree t] <- sampleDyn $ RT.subForest rnode
  rschildren :: [SEltTree] <- mapM serialize children'
  return $ T.Node rslabel rschildren
