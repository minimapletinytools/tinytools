{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}


module Potato.Flow.Reflex.Canvas (
  defaultCanvasLBox
  , Canvas(..)
  , CanvasConfig(..)
  , holdCanvas
) where

import           Relude

import           Reflex

import           Potato.Flow.Math

import           Control.Monad.Fix

defaultCanvasLBox :: LBox
defaultCanvasLBox = LBox (V2 0 0) (V2 100 50)
data Canvas t = Canvas {
  _canvas_box :: Dynamic t LBox
}

data CanvasConfig t = CanvasConfig {
  _canvasConfig_resize :: Event t (LBox -> LBox)
}

holdCanvas :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => CanvasConfig t
  -> m (Canvas t)
holdCanvas CanvasConfig {..} = mdo
  canvasBox <- foldDyn ($) defaultCanvasLBox _canvasConfig_resize
  return
    Canvas {
      _canvas_box = canvasBox
    }
