{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SEltMethods (
  getSEltBox
  , doesSEltIntersectBox
  , updateFnFromController
  , RenderFn
  , SEltDrawer(..)
  , sEltDrawer_renderToLines
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
    SBoxType_BoxText   -> max 0 (width'-2)
    SBoxType_NoBoxText -> width'
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
    (make_lBox_from_XYs (_sSimpleLine_start x) (_sSimpleLine_end x))
    (make_lBox_from_XYs (_sSimpleLine_start x + 1) (_sSimpleLine_end x + 1))
  SEltTextArea x      -> Just $ canonicalLBox_from_lBox_ $ _sTextArea_box x

doesSEltIntersectBox :: LBox -> SElt -> Bool
doesSEltIntersectBox lbox selt = case selt of
  SEltNone                     -> False
  SEltFolderStart              -> False
  SEltFolderEnd                -> False
  SEltBox x                    -> does_lBox_intersect lbox (_sBox_box x)
  SEltTextArea x                   -> does_lBox_intersect lbox (_sTextArea_box x)
  -- TODO this is wrong, do it correctly...
  SEltLine (SSimpleLine start end style lineStyle) -> does_lBox_intersect lbox (fromJust $ getSEltBox (SEltLine (SSimpleLine start end style lineStyle)))



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

sEltDrawer_renderToLines :: SEltDrawer -> [Text]
sEltDrawer_renderToLines SEltDrawer {..} = r where
  LBox (V2 sx sy) (V2 w h) = _sEltDrawer_box
  pts = [[(x,y) | x <- [0..w-1]]| y <- [0..h-1]]
  r' = fmap (fmap (\(x,y) -> fromMaybe ' ' (_sEltDrawer_renderFn (V2 (sx+x) (sy+y))))) pts
  r = fmap T.pack r'


sBox_drawer :: SBox -> SEltDrawer
sBox_drawer sbox@SBox {..} = r where
  CanonicalLBox _ _ lbox@(LBox (V2 x y) (V2 w h)) = canonicalLBox_from_lBox _sBox_box

  fillfn _ = case _superStyle_fill _sBox_style of
    FillStyle_Simple c -> Just c
    FillStyle_Blank    -> Nothing

  rfntext pt@(V2 x' y') = case _sBox_boxType of
    SBoxType_Box -> Nothing
    _ -> outputChar where
      spans = TZ._displayLines_spans $ makeDisplayLinesFromSBox sbox

      (LBox (V2 bx by) _) = _sBox_box
      (xoff,yoff) = case _sBox_boxType of
        SBoxType_NoBoxText -> (0,0)
        _                  -> (1,1)

      outputChar = case spans !!? (y'-by-yoff) of
        Nothing -> Nothing
        Just row -> outputChar' where
          rowText = concatSpans row
          xidx = x'-bx- xoff
          outputChar' = if T.length rowText > xidx
            then Just $ T.index rowText xidx
            else Nothing

  rfnnoborder pt
    | not (does_lBox_contains_XY lbox pt) = Nothing
    | otherwise = case rfntext pt of
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

sSimpleLine_drawer :: SSimpleLine -> SEltDrawer
sSimpleLine_drawer sline@SSimpleLine {..} = r where

  lbox@(LBox (V2 x y) (V2 w h)) = fromJust $ getSEltBox (SEltLine sline)
  xsplit = x + w `div` 2
  ysplit = y + h `div` 2
  V2 startx starty = _sSimpleLine_start
  V2 endx endy = _sSimpleLine_end

  larr = _lineStyle_leftArrows _sSimpleLine_lineStyle
  rarr = T.reverse $ _lineStyle_rightArrows _sSimpleLine_lineStyle
  tarr = _lineStyle_upArrows _sSimpleLine_lineStyle
  barr = T.reverse $ _lineStyle_downArrows _sSimpleLine_lineStyle



  horizrenderfn pt@(V2 x' y')
    | w <= 1 = vertrenderfn pt
    -- render as much of the left arrow as you can and then render the rest
    | isLeft && isLeftHoriz && x'-x < (T.length larr) = Just $ T.index larr (x'-x)
    | isLeft && isLeftHoriz && x' < xsplit = Just $ _superStyle_horizontal _sSimpleLine_style
    -- render as much of the right arrow as you can and then render the rest
    | isRight && isRightHoriz && endx-x' < (T.length larr) = Just $ T.index rarr (endx-x')
    | isRight && isRightHoriz && x' > xsplit = Just $ _superStyle_horizontal _sSimpleLine_style
    -- render the corners
    | isCenter && isTop && isLeftHoriz = Just $ _superStyle_tr _sSimpleLine_style
    | isCenter && isBot && isLeftHoriz = Just $ _superStyle_br _sSimpleLine_style
    | isCenter && isTop && isRightHoriz = Just $ _superStyle_tl _sSimpleLine_style
    | isCenter && isBot && isRightHoriz = Just $ _superStyle_bl _sSimpleLine_style
    -- render the vertical line
    | x' == xsplit = Just $ _superStyle_vertical _sSimpleLine_style
    | otherwise = Nothing
    where
      isLeft = x' < xsplit
      isRight = x' > xsplit
      isLeftHoriz = (y' == starty &&  startx < endx) || (y' == endy && startx > endx)
      isRightHoriz = (y' == starty &&  startx > endx) || (y' == endy && startx < endx)
      isCenter = x' == xsplit
      isTop = (y' == starty && starty < endy) || (y' == endy && starty > endy)
      isBot = (y' == endy && starty < endy) || (y' == starty && starty > endy)



  vertrenderfn pt@(V2 x' y')
    | w <= 1 && h <= 1 = Just $ _superStyle_point _sSimpleLine_style
    | h <= 1 = horizrenderfn pt
    | otherwise = Nothing


  r = SEltDrawer {
      _sEltDrawer_box = lbox
      , _sEltDrawer_renderFn = case _lineStyle_autoStyle _sSimpleLine_lineStyle of
        LineAutoStyle_AutoStraight -> horizrenderfn
        _                          -> error "not implemented"
    }

-- TODO split up so it's not one big case statement...
getDrawer :: SElt -> SEltDrawer
getDrawer selt = case selt of
  SEltNone        -> nilDrawer
  SEltFolderStart -> nilDrawer
  SEltFolderEnd   -> nilDrawer
  SEltBox sbox    -> sBox_drawer sbox
  SEltLine sline  -> sSimpleLine_drawer sline
  SEltTextArea _  -> potatoDrawer
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
  SEltLine sline@SSimpleLine {..} -> SEltLine $ sline {
      _sSimpleLine_start = modifyDelta isDo _sSimpleLine_start
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sSimpleLine_end = modifyDelta isDo _sSimpleLine_end
        (_deltaLBox_translate _cBoundingBox_deltaBox)
    }
  SEltTextArea stext -> SEltTextArea $ stext {
      _sTextArea_box     = modifyDelta isDo (_sTextArea_box stext) _cBoundingBox_deltaBox
    }
  x          -> x

modify_sElt_with_cSuperStyle :: Bool -> SElt -> CSuperStyle -> SElt
modify_sElt_with_cSuperStyle isDo selt (CSuperStyle style) = case selt of
  SEltBox sbox -> SEltBox $ sbox {
      _sBox_style = modifyDelta isDo (_sBox_style sbox) style
    }
  -- TODO handle resize parameter
  SEltLine sline -> SEltLine $ SSimpleLine {
      _sSimpleLine_style = modifyDelta isDo (_sSimpleLine_style sline) style
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
  (CTagBoxText :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltBox s -> SEltLabel sname (SEltBox $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagBoxText - " <> show selt
  (CTagBoxType :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltBox s -> SEltLabel sname (SEltBox $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagBoxText - " <> show selt
  (CTagBoundingBox :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cBoundingBox isDo selt d)
  (CTagSuperStyle :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cSuperStyle isDo selt d)
