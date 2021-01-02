{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SEltMethods (
  getSEltBox
  , doesSEltIntersectBox
  , updateFnFromController
  , RenderFn
  , SEltDrawer(..)
  , getDrawer
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import           Data.Maybe         (fromJust)
import qualified Data.Text          as T
import qualified Data.Text.Zipper   as TZ


makeDisplayLinesFromSBox :: SBox -> TZ.DisplayLines ()
makeDisplayLinesFromSBox sbox = r where
  text = _sBoxText_text . _sBox_text $ sbox
  box@(LBox _ (V2 width' _)) = _sBox_box sbox
  width = case _sBox_boxType sbox of
    SBoxType_BoxText   -> width'
    SBoxType_NoBoxText -> max 0 (width'-2)
    _                  -> error "wrong type"
  r = TZ.displayLines width () () (TZ.fromText text)

concatSpans :: [TZ.Span ()] -> Text
concatSpans spans = mconcat $ fmap (\(TZ.Span _ t) -> t) spans



-- TODO rename to getSEltBoundingBox or something
-- | gets an 'LBox' that contains the entire RElt
getSEltBox :: SElt -> Maybe LBox
getSEltBox selt = case selt of
  SEltNone        -> Nothing
  SEltFolderStart -> Nothing
  SEltFolderEnd   -> Nothing
  -- TODO return canonical
  SEltBox x       -> Just $ canonicalLBox_from_lBox_ $ _sBox_box x
  SEltLine x      -> Just $ union_lBox
    (make_lBox_from_XYs (_sLine_start x) (_sLine_end x))
    (make_lBox_from_XYs (_sLine_start x + 1) (_sLine_end x + 1))
  SEltText x      -> Just $ canonicalLBox_from_lBox_ $ _sText_box x

doesSEltIntersectBox :: LBox -> SElt -> Bool
doesSEltIntersectBox lbox selt = case selt of
  SEltNone                     -> False
  SEltFolderStart              -> False
  SEltFolderEnd                -> False
  SEltBox x                    -> does_lBox_intersect lbox (_sBox_box x)
  SEltText x                   -> does_lBox_intersect lbox (_sText_box x)
  -- TODO this is wrong, do it correctly...
  SEltLine (SLine start end style) -> does_lBox_intersect lbox (fromJust $ getSEltBox (SEltLine (SLine start end style)))



type RenderFn = XY -> Maybe PChar

makePotatoRenderer :: LBox -> RenderFn
makePotatoRenderer lbox pt = if does_lBox_contains_XY lbox pt
  then Just '#'
  else Nothing

data SEltDrawer = SEltDrawer {
  _sEltDrawer_box        :: LBox
  , _sEltDrawer_renderFn :: RenderFn -- switch to [RenderFn] for better performance
}

nilDrawer :: SEltDrawer
nilDrawer = SEltDrawer {
    _sEltDrawer_box = nilLBox
    , _sEltDrawer_renderFn = const Nothing
  }

getDrawer :: SElt -> SEltDrawer
getDrawer selt = case selt of
  SEltNone        -> nilDrawer
  SEltFolderStart -> nilDrawer
  SEltFolderEnd   -> nilDrawer
  SEltBox sbox@SBox {..}       -> r where
    hasBorder = True -- TODO
    CanonicalLBox _ _ lbox@(LBox (V2 x y) (V2 w h)) = canonicalLBox_from_lBox _sBox_box

    fillfn _ = case _superStyle_fill _sBox_style of
      FillStyle_Simple c -> Just c
      FillStyle_Blank    -> Nothing

    rfntext pt@(V2 x' y') = case _sBox_boxType of
      SBoxType_Box -> Nothing
      _ -> outputChar where
        spans = TZ._displayLines_spans $ makeDisplayLinesFromSBox sbox
        (xoff,yoff) = case _sBox_boxType of
          SBoxType_NoBoxText -> (0,0)
          _                  -> (1,1)
        outputChar = case spans !!? (x'-xoff) of
          Nothing -> Nothing
          Just row -> outputChar' where
            rowText = concatSpans row
            yidx = y'-yoff
            outputChar' = if T.length rowText > yidx
              then Just $ T.index rowText yidx
              else Nothing

    rfnnoborder pt = case rfntext pt of
      Just x  -> Just x
      Nothing -> fillfn pt

    rfnborder pt@(V2 x' y')
      | not (does_lBox_contains_XY lbox pt) = Nothing
      | w == 1 && h == 1 = Just $ _superStyle_point _sBox_style
      | w == 1 = Just $ _superStyle_vertical _sBox_style
      | h == 1 = Just $ _superStyle_horizontal _sBox_style
      | x' == x && y' == y = Just $ _superStyle_tl _sBox_style
      | x' == x && y' == y+h-1 = Just $ _superStyle_bl _sBox_style
      | x' == x+w-1 && y' == y = Just $ _superStyle_tr _sBox_style
      | x' == x+w-1 && y' == y+h-1 = Just $ _superStyle_br _sBox_style
      | x' == x || x' == x+w-1 = Just $ _superStyle_vertical _sBox_style
      | y' == y || y' == y+h-1 = Just $ _superStyle_horizontal _sBox_style
      | otherwise = rfnnoborder pt

    r = SEltDrawer {
        _sEltDrawer_box = lbox
        , _sEltDrawer_renderFn = case _sBox_boxType of
          SBoxType_NoBoxText -> rfnnoborder
          _                  -> rfnborder
      }
  SEltLine _      -> potatoDrawer
  SEltText _      -> potatoDrawer
  where
    potatoDrawer = SEltDrawer {
        _sEltDrawer_box = fromJust (getSEltBox selt)
        , _sEltDrawer_renderFn =  makePotatoRenderer $ fromJust (getSEltBox selt)
      }

modify_sElt_with_cBoundingBox :: Bool -> SElt -> CBoundingBox -> SElt
modify_sElt_with_cBoundingBox isDo selt CBoundingBox {..} = case selt of
  SEltBox sbox  -> SEltBox $ sbox {
      _sBox_box = modifyDelta isDo (_sBox_box sbox) _cBoundingBox_deltaBox
    }
  -- TODO handle resize parameter
  SEltLine SLine {..} -> SEltLine $ SLine {
      _sLine_start = modifyDelta isDo _sLine_start
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sLine_end = modifyDelta isDo _sLine_end
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sLine_style = _sLine_style
    }
  SEltText stext -> SEltText $ stext {
      _sText_box     = modifyDelta isDo (_sText_box stext) _cBoundingBox_deltaBox
    }
  x          -> x

modify_sElt_with_cSuperStyle :: Bool -> SElt -> CSuperStyle -> SElt
modify_sElt_with_cSuperStyle isDo selt (CSuperStyle style) = case selt of
  SEltBox sbox -> SEltBox $ sbox {
      _sBox_style = modifyDelta isDo (_sBox_style sbox) style
    }
  -- TODO handle resize parameter
  SEltLine sline -> SEltLine $ SLine {
      _sLine_style = modifyDelta isDo (_sLine_style sline) style
    }
  _ -> error $ "Controller - SElt type mismatch: CTagSuperStyle - " <> show selt
  -- maybe we want silent failure case in the future, so you can easily restyle a big selection in bulk
  --x -> x

modifyDelta :: (Delta x dx) => Bool -> x ->  dx -> x
modifyDelta isDo x dx = if isDo
  then plusDelta x dx
  else minusDelta x dx

updateFnFromController :: Bool -> Controller -> (SEltLabel -> SEltLabel)
updateFnFromController isDo = \case
  (CTagRename :=> Identity d) -> \seltl -> modifyDelta isDo seltl d
  (CTagLine :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltLine s -> SEltLabel sname (SEltLine $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagLine - " <> show selt
  (CTagText :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltBox s -> SEltLabel sname (SEltBox $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagText - " <> show selt
  (CTagBoundingBox :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cBoundingBox isDo selt d)
  (CTagSuperStyle :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cSuperStyle isDo selt d)
