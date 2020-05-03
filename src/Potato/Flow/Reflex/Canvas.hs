{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}


module Potato.Flow.Reflex.Canvas (
  Canvas(..)
  , CanvasConfig(..)
  , holdCanvas
) where

import           Relude

import           Reflex

import           Potato.Flow.Math

import           Control.Monad.Fix

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
  let
    canvasBox0 = LBox (V2 0 0) (V2 100 50)
  canvasBox <- foldDyn ($) canvasBox0 _canvasConfig_resize
  return
    Canvas {
      _canvas_box = canvasBox
    }
