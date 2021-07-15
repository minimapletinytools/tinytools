-- ABANDONED EFFORT
-- This is still a good idea, but you will need to pass in initial state to make this work

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.CanvasWidget (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where

import           Relude

import           Reflex

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Types

import           Control.Monad.Fix
import qualified Data.IntMap                               as IM



data CanvasWidgetConfig t = CanvasWidgetConfig {
  -- connect to goatwidget
  _canvasWidgetConfig_pan                 :: Dynamic t XY
  , _canvasWidgetConfig_broadPhaseWithUpdateSet          :: Dynamic t (NeedsUpdateSet, BroadPhaseState)
  , _canvasWidgetConfig_owlPFState              :: Dynamic t OwlPFState
  , _canvasWidgetConfig_cslmapForRendering              :: Dynamic t SuperOwlChanges

  -- connect to app
  , _canvasWidgetConfig_screenSize :: Dynamic t XY
}

data CanvasWidget t = CanvasWidget {
  -- everything is in absolute coordinates
  _canvasWidget_renderedScreen     :: Dynamic t RenderedCanvasRegion
  , _canvasWidget_canvasRegion :: Dynamic t LBox
}

holdCanvasWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => CanvasWidgetConfig t
  -> m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo

  let
    screenpan = ffor2 (fmap negate _canvasWidgetConfig_pan) _canvasWidgetConfig_screenSize LBox
    inputDyn = ffor2 (ffor3 screenpan _canvasWidgetConfig_broadPhaseWithUpdateSet _canvasWidgetConfig_owlPFState (,,,)) _canvasWidgetConfig_cslmapForRendering (,)

    foldDynFn ((box, (aabbs, bp), pfstate), cslmapForRendering) oldrc = nextrc where
      nextrc' = if _renderedCanvasRegion_box oldrc /= box
        then moveRenderedCanvasRegion bp (_owlPFState_owlTree pfstate) box oldrc
        else oldrc
      nextrc = if IM.null cslmapForRendering
        then nextrc'
        else updateCanvas cslmapForRendering aabbs bp pfstate nextrc'

  --rcDyn <- foldDyn foldDynFn (updated inputDyn)
  rcDyn <- undefined

  return CanvasWidget {
      _canvasWidget_renderedScreen = rcDyn
      , _canvasWidget_canvasRegion = fmap (_sCanvas_box . _owlPFState_canvas) _canvasWidgetConfig_owlPFState
    }
