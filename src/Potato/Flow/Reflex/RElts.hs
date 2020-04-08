module Potato.Flow.Reflex.RElts (

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

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

data REltNode t = REltNode {
  re_elt        :: RElt t
  , re_children :: Dynamic t [REltNode t]
  , re_reflex   :: REltReflex t
}



-- TODO need to pass in add/remove child events throughtout the tree
-- TODO need to pass in elt update events throughout the tree
deserialize :: (Reflex t, MonadHold t m) => SEltNode -> m (Dynamic t (REltNode t))
deserialize (SEltNode selt children) = do
  rchildren <- mapM deserialize children

  -- TODO implement for each type
  (relt, rreflex) <- case selt of
    SEltNone -> return (REltNone, nilReflex)
    _        -> undefined
    --SEltBox x -> hold

  -- TODO
  update <- undefined
  let rnode = REltNode {
      re_elt = relt
      , re_children = sequence rchildren
      , re_reflex = rreflex
    }
  holdDyn rnode update


serialize :: (Reflex t, MonadSample t m) => REltNode t -> m SEltNode
serialize rnode = do
  let
    sampleDyn = sample . current
  rsnode <- case re_elt rnode of
    REltNone   -> return SEltNone
    REltBox x  -> SEltBox <$> sampleDyn x
    REltLine x -> SEltLine <$> sampleDyn x
    REltText x -> SEltText <$> sampleDyn x
  children' :: [REltNode t] <- sampleDyn $ re_children rnode
  rchildren :: [SEltNode] <- mapM serialize children'
  return $ SEltNode rsnode rchildren
