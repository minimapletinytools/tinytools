{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Render (
  Canvas
  , canvas_box
  , emptyCanvas
  , potatoRender
  , canvasToText
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts

import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import qualified Data.Vector.Unboxed      as V


data Canvas = Canvas {
  _canvas_box        :: LBox
  , _canvas_contents :: V.Vector PChar -- ^ row major
}

canvas_box :: Canvas -> LBox
canvas_box = _canvas_box

emptyCanvas :: LBox -> Canvas
emptyCanvas lb@(LBox _ (LSize (V2 w h))) = Canvas {
    _canvas_box = lb
    , _canvas_contents = V.replicate (w*h) ' '
  }

toPoint :: LBox -> Int -> LPoint
toPoint (LBox (LPoint (V2 x y)) (LSize (V2 w _))) i = LPoint $ V2 (i `div` w + x) (i `mod` w + y)

potatoRender :: [SElt] -> Canvas -> Canvas
potatoRender seltls Canvas {..} = r where
  drawers = reverse $ map getDrawer seltls
  imapfn i oc = newc' where
    pt = toPoint _canvas_box i
    -- go through drawers in reverse order until you find a match
    mdraw = find (\d -> isJust $ _sEltDrawer_renderFn d pt) drawers
    newc' = case mdraw of
      Just d  -> fromJust $ _sEltDrawer_renderFn d pt
      Nothing -> oc
  newc = V.imap imapfn _canvas_contents
  r = Canvas {
      _canvas_box = _canvas_box
      , _canvas_contents = newc
    }

canvasToText :: Canvas -> Text
canvasToText Canvas {..} = T.unfoldr unfoldfn (0, False) where
  l = V.length _canvas_contents
  (LBox _ (LSize (V2 w _))) = _canvas_box
  unfoldfn (i, eol) = if i == l
    then Nothing
    else if eol
      then Just $ ('\n', (i, False))
      else if (i+1) `mod` w == 0
        then Just $ (_canvas_contents V.! i, (i+1, True))
        else Just $ (_canvas_contents V.! i, (i+1, False))
