{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextArea (
  TextAreaHandler(..)
  , makeTextAreaHandler
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.Owl
import           Potato.Flow.OwlWorkspace

import           Data.Dependent.Sum                        (DSum ((:=>)))
import Data.Default
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.Text as T


getSTextArea :: CanvasSelection -> (REltId, STextArea)
getSTextArea selection = case superOwl_toSElt_hack sowl of
  SEltTextArea stextarea -> (rid, stextarea)
  selt -> error $ "expected SBox, got " <> show selt
  where
    sowl = selectionToSuperOwl selection
    rid = _superOwl_id sowl

data TextAreaHandler = TextAreaHandler {
    _textAreaHandler_prevHandler :: SomePotatoHandler
    , _textAreaHandler_relCursor :: XY
  }

getCursorPosHelper :: CanvasSelection -> RelMouseDrag -> (XY, Bool)
getCursorPosHelper selection rmd@(RelMouseDrag MouseDrag {..}) = r where
  (_, STextArea {..}) = getSTextArea selection
  CanonicalLBox _ _ lbox@(LBox p (V2 _ _)) = canonicalLBox_from_lBox _sTextArea_box
  newrelpos = _mouseDrag_to - p
  clickinside = does_lBox_contains_XY lbox _mouseDrag_to
  r = (newrelpos, clickinside)

makeTextAreaHandler :: SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> Bool -> TextAreaHandler
makeTextAreaHandler prev selection rmd creation = r where
  (newrelpos, _) = getCursorPosHelper selection rmd
  r = TextAreaHandler {
    _textAreaHandler_prevHandler = prev
    -- we want the cursor at the beginning if we are creating TextAreaHandler right after creating a new text area
    , _textAreaHandler_relCursor = if creation then 0 else newrelpos
  }

instance PotatoHandler TextAreaHandler where
  pHandlerName _ = handlerName_textArea
  pHandlerDebugShow tah = "TextAreaHandler, cursor: " <> show (_textAreaHandler_relCursor tah)
  pHandleMouse tah phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      (newrelpos, clickinside) = getCursorPosHelper _potatoHandlerInput_canvasSelection rmd
    in
      case _mouseDrag_state of
        MouseDragState_Down -> r where
          r = if clickinside
            then Just $ def {
                _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                    _textAreaHandler_relCursor = newrelpos
                  }
              }
            -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
            else pHandleMouse (_textAreaHandler_prevHandler tah) phi rmd

        -- TODO "painting" mode someday
        MouseDragState_Dragging -> Just $ captureWithNoChange tah
        MouseDragState_Up -> Just $ captureWithNoChange tah
        MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah PotatoHandlerInput {..} (KeyboardData k _) = let
      (rid, STextArea {..}) = getSTextArea _potatoHandlerInput_canvasSelection
      CanonicalLBox _ _ lbox@(LBox _ (V2 width height)) = canonicalLBox_from_lBox _sTextArea_box
      wrapBox (V2 x y) = V2 (x `mod` width) (y `mod` height)


      getCursorChar h = Map.lookup (_textAreaHandler_relCursor h) _sTextArea_text
      -- combinators
      start = (Map.empty, tah)
      finish (mc, h) = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler h
          , _potatoHandlerOutput_pFEvent = if null mc
            then Nothing
            -- TODO if you store mc in TextAreaHandler you can continue to build on it which would allow you to set "undoFirst" paremeter to True
            else Just $ WSEManipulate (False, IM.singleton rid controller)
        } where
          controller = CTagTextArea :=> (Identity $ CTextArea (DeltaTextArea mc))
      moveAndWrap dp (mc, h) = (mc, h {
          _textAreaHandler_relCursor = wrapBox $ (_textAreaHandler_relCursor tah) + dp
        })
      -- TODO
      setChar c (mc, h) = (Map.insert (_textAreaHandler_relCursor h) (getCursorChar h, Just c) mc, h)
      deleteChar (mc, h) = (Map.insert (_textAreaHandler_relCursor h) (getCursorChar h, Nothing) mc, h)


    in case k of
      KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_textAreaHandler_prevHandler tah) }
      KeyboardKey_Left -> finish . moveAndWrap (V2 (-1) 0) $ start
      KeyboardKey_Right -> finish .  moveAndWrap (V2 1 0) $ start
      KeyboardKey_Down -> finish . moveAndWrap (V2 0 1) $ start
      KeyboardKey_Up -> finish . moveAndWrap (V2 0 (-1)) $ start
      KeyboardKey_Return  -> finish . moveAndWrap (V2 0 1) $ start
      KeyboardKey_Space   -> finish . moveAndWrap (V2 1 0) . setChar ' ' $ start
      KeyboardKey_Delete  -> finish . deleteChar $ start
      KeyboardKey_Backspace -> finish . deleteChar . moveAndWrap (V2 (-1) 0) $ start
      KeyboardKey_Char c  -> finish . moveAndWrap (V2 1 0) . setChar c $ start
      KeyboardKey_Paste t -> finish $ foldl' (\acc c -> moveAndWrap (V2 1 0) . setChar c $ acc) start (T.unpack t)
      _ -> Nothing

  pRefreshHandler tah PotatoHandlerInput {..} = Nothing
  pRenderHandler tah phi@PotatoHandlerInput {..} = r where

    -- TODO maybe store instead of pull from selection?
    (_, STextArea {..}) = getSTextArea _potatoHandlerInput_canvasSelection
    CanonicalLBox _ _ lbox@(LBox p (V2 _ _)) = canonicalLBox_from_lBox _sTextArea_box
    cursor = RenderHandle {
        _renderHandle_box = LBox (p + _textAreaHandler_relCursor tah) (V2 1 1)
        , _renderHandle_char = Map.lookup (_textAreaHandler_relCursor tah) _sTextArea_text
      }
    r = pRenderHandler (_textAreaHandler_prevHandler tah) phi <>  HandlerRenderOutput [cursor]
  pIsHandlerActive tah = False
