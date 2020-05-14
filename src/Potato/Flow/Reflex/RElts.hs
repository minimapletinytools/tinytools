{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Reflex.RElts (
  getSEltBox
  , updateFnFromController
  , RenderFn
  , SEltDrawer(..)
  , getDrawer
  , Selected
  , toManipulator
) where

import           Relude
import           Relude.Extra.Foldable1   (foldl1')

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Monad.Fix

import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import           Data.Maybe               (fromJust)
import           Data.Tuple.Extra

import           Reflex


-- TODO rename to getSEltBoundingBox or something
-- | gets an 'LBox' that contains the entire RElt
getSEltBox :: SElt -> Maybe LBox
getSEltBox selt = case selt of
  SEltNone        -> Nothing
  SEltFolderStart -> Nothing
  SEltFolderEnd   -> Nothing
  -- TODO return canonical
  SEltBox x       -> Just $ canonicalLBox_from_lBox_ $ _sBox_box x
  SEltLine x      -> Just $ make_LBox_from_XYs (_sLine_start x) (_sLine_end x)
  SEltText x      -> Just $ canonicalLBox_from_lBox_ $ _sText_box x

type RenderFn = XY -> Maybe PChar

makePotatoRenderer :: LBox -> RenderFn
makePotatoRenderer lbox pt = if does_LBox_contains_XY lbox pt
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
  SEltBox SBox {..}       -> r where
    CanonicalLBox _ _ lbox@(LBox (V2 x y) (V2 w h)) = canonicalLBox_from_lBox _sBox_box
    rfn pt@(V2 x' y')
      | not (does_LBox_contains_XY lbox pt) = Nothing
      | w == 1 && h == 1 = Just $ _superStyle_point _sBox_style
      | w == 1 = Just $ _superStyle_vertical _sBox_style
      | h == 1 = Just $ _superStyle_horizontal _sBox_style
      | x' == x && y' == y = Just $ _superStyle_tl _sBox_style
      | x' == x && y' == y+h-1 = Just $ _superStyle_bl _sBox_style
      | x' == x+w-1 && y' == y = Just $ _superStyle_tr _sBox_style
      | x' == x+w-1 && y' == y+h-1 = Just $ _superStyle_br _sBox_style
      | x' == x || x' == x+w-1 = Just $ _superStyle_vertical _sBox_style
      | y' == y || y' == y+h-1 = Just $ _superStyle_horizontal _sBox_style
      | otherwise = case _superStyle_fill _sBox_style of
        FillStyle_Simple c -> Just c
        FillStyle_Blank    -> Nothing
    r = SEltDrawer {
        _sEltDrawer_box = lbox
        , _sEltDrawer_renderFn = rfn
      }
  SEltLine _      -> potatoDrawer
  SEltText _      -> potatoDrawer
  where
    potatoDrawer = SEltDrawer {
        _sEltDrawer_box = fromJust (getSEltBox selt)
        , _sEltDrawer_renderFn =  makePotatoRenderer $ fromJust (getSEltBox selt)
      }

-- TODO delete this type synonym, not useful
type Selected = [SuperSEltLabel]

toManipulator :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Event t Selected -- ^ selection event, which will sample manipulators of current selection
  -> m (Dynamic t Manipulator)
toManipulator selected = do
  let
    nilState :: Manipulator
    nilState = (MTagNone ==> ())
    foldfn :: Selected -> Manipulator -> Manipulator
    foldfn [] _ = nilState
    foldfn ((rid, _, SEltLabel _ selt):[]) _ = case selt of
      SEltBox SBox {..} -> (MTagBox ==> mbox) where
        mbox = MBox {
            _mBox_target = rid
            , _mBox_box  = _sBox_box
          }
      SEltLine SLine {..} -> (MTagLine ==> mline) where
        mline = MLine {
            _mLine_target  = rid
            , _mLine_start = _sLine_start
            , _mLine_end   = _sLine_end
          }
      SEltText SText {..} -> (MTagText ==> mtext) where
        mtext = MText {
            _mText_target = rid
            , _mText_box  = _sText_box
            , _mText_text = _sText_text
          }
      _                 -> nilState
    foldfn sss _ = r where
      fmapfn (rid, _, seltl) = do
        box <- getSEltBox . _sEltLabel_sElt $ seltl
        return (rid,box)
      msboxes = catMaybes $ fmap fmapfn sss
      r = fromMaybe nilState $ flip viaNonEmpty msboxes $ \sboxes ->
        MTagBoundingBox ==>
          MBoundingBox {
            _mBoundingBox_bounded_targets = sboxes
          }
  foldDyn foldfn nilState selected


modify_sElt_with_cBoundingBox :: Bool -> SElt -> CBoundingBox -> SElt
modify_sElt_with_cBoundingBox isDo selt CBoundingBox {..} = case selt of
  SEltBox SBox {..}  -> SEltBox $ SBox {
      _sBox_box = modifyDelta isDo _sBox_box _cBoundingBox_deltaBox
      , _sBox_style = _sBox_style
    }
  -- TODO handle resize parameter
  SEltLine SLine {..} -> SEltLine $ SLine {
      _sLine_start = modifyDelta isDo _sLine_start
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sLine_end = modifyDelta isDo _sLine_end
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sLine_style = _sLine_style
    }
  SEltText SText {..} -> SEltText $ SText {
      _sText_box     = modifyDelta isDo _sText_box _cBoundingBox_deltaBox
      , _sText_text  = _sText_text
      , _sText_style = _sText_style
    }
  x          -> x


modifyDelta :: (Delta x dx) => Bool -> x ->  dx -> x
modifyDelta isDo x dx = if isDo
  then plusDelta x dx
  else minusDelta x dx

updateFnFromController :: Bool -> Controller -> (SEltLabel -> SEltLabel)
updateFnFromController isDo = \case
  (CTagRename :=> Identity d) -> \seltl -> modifyDelta isDo seltl d
  (CTagBox :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltBox s -> SEltLabel sname (SEltBox $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagBox - " <> show selt
  (CTagLine :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltLine s -> SEltLabel sname (SEltLine $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagLine - " <> show selt
  (CTagText :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltText s -> SEltLabel sname (SEltText $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagText - " <> show selt
  (CTagBoundingBox :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cBoundingBox isDo selt d)
  _ -> id









-- TODO DELETE

{-
-- needed by 'toManipulator' internally

data SEltTag a where
  SEltTagNone :: SEltTag ()
  SEltTagBox :: SEltTag SBox
  SEltTagLine :: SEltTag SLine
  SEltTagText :: SEltTag SText

  -- TODO TH to derive these
  --deriving anyclass Data.GADT.Compare.GEq
  --deriving anyclass DM.GCompare
-}



{-
deserializeRElt ::
  forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => ControllerEventSelector t -- ^ event selector for do action
  -> ControllerEventSelector t -- ^ event selector for undo action
  -> (REltId, SEltLabel)
  -> m (REltLabel t)
deserializeRElt doSelector undoSelector (reltid, SEltLabel sname selt) = do

  let
    reltDoEv = selectInt doSelector reltid
    reltUndoEv = selectInt undoSelector reltid
    bothEv :: Event t (Either Controller Controller)
    bothEv = alignEitherWarn ("("<>show reltid<>") do/undo") reltDoEv reltUndoEv

  -- TODO implement for each type
  relt <- case selt of
    SEltNone        -> return REltNone
    SEltFolderStart -> return REltFolderStart
    SEltFolderEnd   -> return REltFolderEnd
    SEltBox SBox {..} -> do
      let
        foldfn :: Either Controller Controller -> LBox -> LBox
        foldfn (Left ct) box = case ct of
          (SEltTagBox :=> Identity dbox) -> plusDelta box dbox
        foldfn (Right ct) box = case ct of
          (SEltTagBox :=> Identity dbox) -> minusDelta box dbox
      mbox <- foldDyn foldfn _sBox_box bothEv
      return $ REltBox (MBox mbox)

{-
    -- TODO wut a pain DDDDD:
    SEltLine SLine {..} -> do
      let
        foldfn :: Either Controller Controller -> LPoint -> LPoint
        foldfn
      --sl_start   :: LPoint
      --, sl_end  ::LPoint
      mline <- MLine startl endl
-}

    _               -> undefined
  return $ REltLabel sname relt
-}



{-
type ManipulatorWithId t = DS.DSum (Const2 LayerEltId (Manipulators t)) Identity
type ControllerWithId = DS.DSum (Const2 LayerEltId Controller) Identity
type ControllerEventSelector t = EventSelector t (Const2 LayerEltId Controller)
-}

{-

data RElt t =
  REltNone
  | REltFolderStart
  | REltFolderEnd
  | REltBox (MBox t)
  | REltLine (MLine t)
  | REltText (MText t)

getREltManipulator :: RElt t -> Manipulators t
getREltManipulator relt = case relt of
  REltNone        -> none
  REltFolderStart -> none
  REltFolderEnd   -> none
  REltBox x       -> MTagBox ==> x
  REltLine x      -> MTagLine ==> x
  REltText x      -> MTagText ==> x
  where
    none = MTagNone ==> ()


data REltLabel t = REltLabel {
  re_name  :: Text
  , re_elt :: RElt t
}
-}
