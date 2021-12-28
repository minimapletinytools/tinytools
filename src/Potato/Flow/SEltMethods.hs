{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SEltMethods (
  getSEltBox
  , getSEltLabelBox
  , getSEltSuperStyle
  , getSEltLabelSuperStyle
  , getSEltLineStyle
  , getSEltLabelLineStyle
  , getSEltBoxTextStyle
  , getSEltLabelBoxTextStyle
  , getSEltBoxType
  , getSEltLabelBoxType
  , doesSEltIntersectBox
  , doesSEltIntersectPoint
  , updateFnFromController
  , getDrawer
  , offsetSEltTree
) where

import           Relude
import  qualified         Relude.Unsafe

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import Potato.Flow.Methods.Types

import qualified Data.Map as Map
import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import           Data.Maybe         (fromJust)
import qualified Data.Text          as T
import qualified Potato.Data.Text.Zipper   as TZ


noTrailngCursorDisplayLines :: Int -> TextAlign -> T.Text -> TZ.DisplayLines Int
noTrailngCursorDisplayLines width alignment text = r where
  -- force TZ to top so that displayLinesWithAlignment doesn't create trailing space for cursor
  tz = TZ.top (TZ.fromText text)

  -- hack to get rid of trailing cursor if text is ""
  r = if T.null text
    then TZ.DisplayLines {
        _displayLines_spans = []
        , _displayLines_offsetMap = Map.empty
        , _displayLines_cursorPos   = (0,0)
      }
    else TZ.displayLinesWithAlignment (convertTextAlignToTextZipperTextAlignment alignment) width 0 1 tz

makeDisplayLinesFromSBox :: SBox -> TZ.DisplayLines Int
makeDisplayLinesFromSBox sbox = r where
  alignment = _textStyle_alignment . _sBoxText_style . _sBox_text $ sbox
  text = _sBoxText_text . _sBox_text $ sbox
  box@(LBox _ (V2 width' _)) = _sBox_box sbox
  width = case _sBox_boxType sbox of
    SBoxType_BoxText   -> max 0 (width'-2)
    SBoxType_NoBoxText -> width'
    _                  -> error "wrong type"
  r = noTrailngCursorDisplayLines width alignment text

concatSpans :: [TZ.Span a] -> Text
concatSpans spans = mconcat $ fmap (\(TZ.Span _ t) -> t) spans

subWidth :: Text -> [Maybe Char]
subWidth t = join . fmap fn . T.unpack $ t where
  fn c = case TZ.charWidth c of
    1 -> [Just c]
    2 -> [Just c, Nothing]
    n -> trace ("unexpected char " <> [c] <> " of width " <> show n) [Nothing]



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

getSEltLabelBox :: SEltLabel -> Maybe LBox
getSEltLabelBox (SEltLabel _ x) = getSEltBox x

doesSEltIntersectBox :: LBox -> SElt -> Bool
doesSEltIntersectBox lbox selt = case selt of
  SEltNone                     -> False
  SEltFolderStart              -> False
  SEltFolderEnd                -> False
  SEltBox x                    -> does_lBox_intersect_include_zero_area lbox (_sBox_box x)
  SEltTextArea x                   -> does_lBox_intersect_include_zero_area lbox (_sTextArea_box x)
  -- TODO this is wrong, do it correctly...
  -- we use does_lBox_intersect since it's impossibl efor a SSimpleLine to have zero sized box
  SEltLine sline@SSimpleLine {..} -> does_lBox_intersect lbox (fromJust $ getSEltBox (SEltLine sline))

doesSEltIntersectPoint :: XY -> SElt -> Bool
doesSEltIntersectPoint pos selt = doesSEltIntersectBox (LBox pos (V2 1 1)) selt

getSEltSuperStyle :: SElt -> Maybe SuperStyle
getSEltSuperStyle selt = case selt of
  SEltBox SBox {..}         -> Just _sBox_style
  SEltLine SSimpleLine {..} -> Just _sSimpleLine_style
  _                         -> Nothing

getSEltLabelSuperStyle :: SEltLabel -> Maybe SuperStyle
getSEltLabelSuperStyle (SEltLabel _ x) = getSEltSuperStyle x

getSEltLineStyle :: SElt -> Maybe LineStyle
getSEltLineStyle selt = case selt of
  SEltLine SSimpleLine {..} -> Just _sSimpleLine_lineStyle
  _                         -> Nothing

getSEltLabelLineStyle :: SEltLabel -> Maybe LineStyle
getSEltLabelLineStyle (SEltLabel _ x) = getSEltLineStyle x

getSEltBoxTextStyle :: SElt -> Maybe TextStyle
getSEltBoxTextStyle = \case
  SEltBox SBox {..}         -> Just . _sBoxText_style $ _sBox_text
  _ -> Nothing

getSEltLabelBoxTextStyle :: SEltLabel -> Maybe TextStyle
getSEltLabelBoxTextStyle (SEltLabel _ x) = getSEltBoxTextStyle x

getSEltBoxType :: SElt -> Maybe SBoxType
getSEltBoxType = \case
  SEltBox SBox {..} -> Just _sBox_boxType
  _ -> Nothing

getSEltLabelBoxType :: SEltLabel -> Maybe SBoxType
getSEltLabelBoxType (SEltLabel _ x) = getSEltBoxType x




sBox_drawer :: SBox -> SEltDrawer
sBox_drawer sbox@SBox {..} = r where
  CanonicalLBox _ _ lbox@(LBox (V2 x y) (V2 w h)) = canonicalLBox_from_lBox _sBox_box

  titlewidth = max 0 (w-2)

  fillfn _ = case _superStyle_fill _sBox_style of
    FillStyle_Simple c -> Just c
    FillStyle_Blank    -> Nothing

  displayLinesToChar dl (x',y') (xoff, yoff) = outputChar where
    spans = TZ._displayLines_spans dl
    offsetMap = TZ._displayLines_offsetMap dl
    yidx = y' - y - yoff
    xalignoffset = case Map.lookup yidx offsetMap of
      Nothing -> error $ "should not happen. got " <> show yidx <> " in\n" <> show dl <> "\n" <> show spans <> "\n" <> show (_sBox_text) <> show _sBox_box
      Just (offset,_) -> offset
    outputChar = case spans !!? yidx of
      Nothing -> Nothing
      Just row -> outputChar' where
        rowText = subWidth $ concatSpans row
        xidx = x' - x - xoff - xalignoffset
        outputChar' = case rowText !!? xidx of
          Nothing -> Nothing
          Just x -> Just x

  rfntext pt@(V2 x' y') = case _sBox_boxType of
    SBoxType_Box -> Nothing
    SBoxType_NoBox -> Nothing
    _ -> outputChar where

      -- 😰😰😰 for now we just do the below for every cell
      dl = makeDisplayLinesFromSBox sbox

      offs = case _sBox_boxType of
        SBoxType_NoBoxText -> (0,0)
        _                  -> (1,1)

      outputChar = displayLinesToChar dl (x', y') offs

  -- TODO test
  rfnlabel pt@(V2 x' y') = case _sBoxTitle_title _sBox_title of
    Nothing -> Nothing
    Just title -> outputChar where
      -- TODO we want to crop instead of wrap here
      -- however using infinite width trick will break AlignRight :(
      dl = noTrailngCursorDisplayLines titlewidth (_sBoxTitle_align _sBox_title) title
      -- note that y' will ultimately resolve to a yindex of 0 inside of displayLinesToChar
      outputChar = displayLinesToChar dl (x', y') (1,0)

  rfnnoborder pt
    | not (does_lBox_contains_XY lbox pt) = Nothing
    | otherwise = case rfntext pt of
      -- 'Just Nothing' means don't use fill char (this happens when there are wide chars)
      Just mx  -> mx
      Nothing -> fillfn pt

  rfnborder pt@(V2 x' y')
    | not (does_lBox_contains_XY lbox pt) = Nothing
    | w == 1 && h == 1 = _superStyle_point _sBox_style
    | w == 1 = _superStyle_vertical _sBox_style
    | h == 1 = _superStyle_horizontal _sBox_style
    | x' == x && y' == y = _superStyle_tl _sBox_style
    | x' == x && y' == y+h-1 = _superStyle_bl _sBox_style
    | x' == x+w-1 && y' == y = _superStyle_tr _sBox_style
    | x' == x+w-1 && y' == y+h-1 = _superStyle_br _sBox_style
    | x' == x || x' == x+w-1 = _superStyle_vertical _sBox_style
    -- label shows up at top horizontal portion
    | y' == y = case rfnlabel pt of
      Nothing -> _superStyle_horizontal _sBox_style
      Just x -> x
    | y' == y+h-1 = _superStyle_horizontal _sBox_style
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
    | not (does_lBox_contains_XY lbox pt) = Nothing
    | w <= 1 = vertrenderfn pt
    -- render as much of the left arrow as you can and then render the rest
    | isLeft && isLeftHoriz && x'-x < (T.length larr) = Just $ T.index larr (x'-x)
    | isLeft && isLeftHoriz = _superStyle_horizontal _sSimpleLine_style
    -- render as much of the right arrow as you can and then render the rest
    | isRight && isRightHoriz && farend-x' < (T.length rarr) = Just $ T.index rarr (farend-x')
    | isRight && isRightHoriz = _superStyle_horizontal _sSimpleLine_style
    -- render the corners
    | isCenter && isTop && isLeftHoriz = _superStyle_tr _sSimpleLine_style
    | isCenter && isBot && isLeftHoriz = _superStyle_br _sSimpleLine_style
    | isCenter && isTop && isRightHoriz = _superStyle_tl _sSimpleLine_style
    | isCenter && isBot && isRightHoriz = _superStyle_bl _sSimpleLine_style
    -- special case
    | h == 1 = _superStyle_horizontal _sSimpleLine_style
    -- render the vertical line
    | isCenter = _superStyle_vertical _sSimpleLine_style
    | otherwise = Nothing
    where
      isLeft = x' < xsplit
      isRight = x' > xsplit
      isLeftHoriz = (y' == starty &&  startx < endx) || (y' == endy && startx > endx)
      isRightHoriz = (y' == starty &&  startx > endx) || (y' == endy && startx < endx)
      isCenter = x' == xsplit
      isTop = (y' == starty && starty < endy) || (y' == endy && starty > endy)
      isBot = (y' == endy && starty < endy) || (y' == starty && starty > endy)
      farend = max startx endx

  vertrenderfn pt@(V2 x' y')
    | not (does_lBox_contains_XY lbox pt) = Nothing
    | w <= 1 && h <= 1 = _superStyle_point _sSimpleLine_style
    | h <= 1 = horizrenderfn pt
    -- render as much of the top arrow as you can and then render the rest
    | isTop && isTopVert && y'-y < (T.length tarr) = Just $ T.index tarr (y'-y)
    | isTop && isTopVert = _superStyle_vertical _sSimpleLine_style
    -- render as much of the right arrow as you can and then render the rest
    | isBot && isBotVert && farend-y' < (T.length barr) = Just $ T.index barr (farend-y')
    | isBot && isBotVert = _superStyle_vertical _sSimpleLine_style
    -- render the corners
    | isCenter && isLeft && isTopVert = _superStyle_bl _sSimpleLine_style
    | isCenter && isRight && isTopVert = _superStyle_br _sSimpleLine_style
    | isCenter && isLeft && isBotVert = _superStyle_tl _sSimpleLine_style
    | isCenter && isRight && isBotVert = _superStyle_tr _sSimpleLine_style
    -- special case
    | w == 1 = _superStyle_vertical _sSimpleLine_style
    -- render the horizontal line
    | isCenter = _superStyle_horizontal _sSimpleLine_style
    | otherwise = Nothing
    where
      isLeft = (x' == startx && startx < endx) || (x' == endx && startx > endx)
      isRight = (x' == endx && startx < endx) || (x' == startx && startx > endx)
      isTopVert = (x' == startx &&  starty < endy) || (x' == endx && starty > endy)
      isBotVert = (x' == startx &&  starty > endy) || (x' == endx && starty < endy)
      isCenter = y' == ysplit
      isTop = y' < ysplit
      isBot = y' > ysplit
      farend = (max starty endy)

  autorenderfn = if w >= h then horizrenderfn else vertrenderfn

  r = SEltDrawer {
      _sEltDrawer_box = lbox
      , _sEltDrawer_renderFn = case _lineStyle_autoStyle _sSimpleLine_lineStyle of
        LineAutoStyle_Auto                     -> autorenderfn
        LineAutoStyle_AutoStraight             -> autorenderfn
        LineAutoStyle_StraightAlwaysHorizontal -> horizrenderfn
        LineAutoStyle_StraightAlwaysHorizontal -> vertrenderfn
    }

sTextArea_drawer :: STextArea -> SEltDrawer
sTextArea_drawer stextarea@STextArea {..} = r where

  lbox@(LBox p (V2 w h)) = _sTextArea_box

  renderfn p' = outputChar where
    inbounds = does_lBox_contains_XY lbox p'
    outputChar = if inbounds
      then case Map.lookup (p' - p) _sTextArea_text of
        Nothing -> if _sTextArea_transparent
          then Nothing
          else Just ' '
        Just c -> Just c
      else Nothing

  r = SEltDrawer {
      _sEltDrawer_box = lbox
      , _sEltDrawer_renderFn = renderfn
    }





-- TODO split up so it's not one big case statement...
getDrawer :: SElt -> SEltDrawer
getDrawer selt = case selt of
  SEltNone        -> nilDrawer
  SEltFolderStart -> nilDrawer
  SEltFolderEnd   -> nilDrawer
  SEltBox sbox    -> sBox_drawer sbox
  SEltLine sline  -> sSimpleLine_drawer sline
  SEltTextArea stextarea  -> sTextArea_drawer stextarea
  {-
  where
    potatoDrawer = SEltDrawer {
        _sEltDrawer_box = fromJust (getSEltBox selt)
        , _sEltDrawer_renderFn =  makePotatoRenderer $ fromJust (getSEltBox selt)
      }
  -}

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
  SEltLine sline -> SEltLine $ sline {
      _sSimpleLine_style = modifyDelta isDo (_sSimpleLine_style sline) style
    }
  _ -> error $ "Controller - SElt type mismatch: CTagSuperStyle - " <> show selt
  -- maybe we want silent failure case in the future, so you can easily restyle a big selection in bulk
  --x -> x

modify_sElt_with_cLineStyle :: Bool -> SElt -> CLineStyle -> SElt
modify_sElt_with_cLineStyle isDo selt (CLineStyle style) = case selt of
  SEltLine sline -> SEltLine $ sline {
      _sSimpleLine_lineStyle = modifyDelta isDo (_sSimpleLine_lineStyle sline) style
    }
  _ -> error $ "Controller - SElt type mismatch: CTagLineStyle - " <> show selt
  -- maybe we want silent failure case in the future, so you can easily restyle a big selection in bulk
  --x -> x

modify_sElt_with_cTextStyle :: Bool -> SElt -> CTextStyle -> SElt
modify_sElt_with_cTextStyle isDo selt (CTextStyle style) = case selt of
  SEltBox sbox -> SEltBox $ sbox {
      _sBox_text = (_sBox_text sbox) {
          _sBoxText_style = modifyDelta isDo (_sBoxText_style . _sBox_text $ sbox) style
        }
    }
  _ -> error $ "Controller - SElt type mismatch: CTagBoxTextStyle - " <> show selt
  -- maybe we want silent failure case in the future, so you can easily restyle a big selection in bulk
  --x -> x

modify_sEltBox_label_with_cTextAlign :: Bool -> SElt -> CTextAlign -> SElt
modify_sEltBox_label_with_cTextAlign isDo selt (CTextAlign align) = case selt of
  SEltBox sbox -> SEltBox $ sbox {
      _sBox_title = sboxtitle {
          _sBoxTitle_align = modifyDelta isDo (_sBoxTitle_align sboxtitle) align
        }
    } where
      sboxtitle = _sBox_title sbox
  _ -> error $ "Controller - SElt type mismatch: CTagBoxLabelAlignment - " <> show selt
  -- maybe we want silent failure case in the future, so you can easily restyle a big selection in bulk
  --x -> x

modify_sEltBox_label_with_cMaybeText :: Bool -> SElt -> CMaybeText -> SElt
modify_sEltBox_label_with_cMaybeText isDo selt (CMaybeText text) = case selt of
  SEltBox sbox -> SEltBox $ sbox {
      _sBox_title = sboxtitle {
          _sBoxTitle_title = modifyDelta isDo (_sBoxTitle_title sboxtitle) text
        }
    } where
      sboxtitle = _sBox_title sbox
  _ -> error $ "Controller - SElt type mismatch: CTagBoxLabelAlignment - " <> show selt

modify_sEltTextArea_with_cTextArea :: Bool -> SElt -> CTextArea -> SElt
modify_sEltTextArea_with_cTextArea isDo selt (CTextArea dt) = case selt of
  SEltTextArea stextarea -> SEltTextArea $ stextarea {
      _sTextArea_text = modifyDelta isDo (_sTextArea_text stextarea) dt
    }
  _ -> error $ "Controller - SElt type mismatch: CTagTextArea - " <> show selt

modify_sEltTextArea_with_cTextAreaToggle :: Bool -> SElt -> CTextAreaToggle -> SElt
modify_sEltTextArea_with_cTextAreaToggle isDo selt (CTextAreaToggle toggle) = case selt of
  -- double toggle is idempotent but we disallow it for now
  SEltTextArea _ -> error $ "Controller - SElt type mismatch: CTagTextAreaToggle - " <> show selt
  x -> modifyDelta isDo x toggle


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
  (CTagLineStyle :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cLineStyle isDo selt d)
  (CTagBoxTextStyle :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cTextStyle isDo selt d)

  (CTagBoxLabelAlignment :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sEltBox_label_with_cTextAlign isDo selt d)
  (CTagBoxLabelText :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sEltBox_label_with_cMaybeText isDo selt d)

  (CTagTextArea :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sEltTextArea_with_cTextArea isDo selt d)
  (CTagTextAreaToggle :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sEltTextArea_with_cTextAreaToggle isDo selt d)


-- | helper method used in copy pasta
offsetSEltTree :: XY -> SEltTree -> SEltTree
offsetSEltTree offset stree = r where
  op = CBoundingBox (DeltaLBox offset 0)
  offsetfn (rid, seltl) = (rid, updateFnFromController True (CTagBoundingBox :=> Identity op) seltl)
  r = fmap offsetfn stree
