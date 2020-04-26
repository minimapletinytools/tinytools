{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Reflex.RElts (
  getSEltBox
  , updateFnFromController
  , Renderer
  , getDrawer
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


-- | gets an 'LBox' that contains the entire RElt
getSEltBox :: SElt -> Maybe LBox
getSEltBox selt = case selt of
  SEltNone        -> Nothing
  SEltFolderStart -> Nothing
  SEltFolderEnd   -> Nothing
  SEltBox x       -> Just $ _sBox_box x
  SEltLine x      -> Just $ make_LBox_from_LPoints (_sLine_start x) (_sLine_end x)
  SEltText x      -> Just $ _sText_box x

data Renderer = Renderer LBox (LPoint -> Maybe PChar)

makePotatoRenderer :: LBox -> Renderer
makePotatoRenderer lbox = Renderer lbox $ \p -> if does_LBox_contains_LPoint lbox p
  then Just '#'
  else Nothing

data SEltDrawer = SEltDrawer {
  _sEltDrawer_box        :: LBox
  , _sEltDrawer_renderer :: Renderer -- switch to [Renderer] for better performance
}

nilDrawer :: SEltDrawer
nilDrawer = SEltDrawer {
    _sEltDrawer_box = nilLBox
    , _sEltDrawer_renderer = Renderer nilLBox (const Nothing)
  }

getDrawer :: SElt -> SEltDrawer
getDrawer selt = case selt of
  SEltNone        -> nilDrawer
  SEltFolderStart -> nilDrawer
  SEltFolderEnd   -> nilDrawer
  SEltBox _       -> potatoDrawer
  SEltLine _      -> potatoDrawer
  SEltText _      -> potatoDrawer
  where
    potatoDrawer = SEltDrawer {
        _sEltDrawer_box = fromJust (getSEltBox selt)
        , _sEltDrawer_renderer =  makePotatoRenderer $ fromJust (getSEltBox selt)
      }

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
    foldfn (s:ss) _ = r where
      nes = s:|ss
      msboxes = catMaybes . toList $ fmap (getSEltBox . _sEltLabel_sElt . thd3) nes
      r = fromMaybe nilState $ flip viaNonEmpty msboxes $ \sboxes ->
        MTagRelBox ==>
          MRelBox {
            _mRelBox_targets    = fmap fst3 nes
            , _mRelBox_box      = foldl1' union_LBox sboxes
          }
  foldDyn foldfn nilState selected




{-
scale_LSize :: (Float,Float) -> LSize -> LSize
scale_LSize (sx, sy) (LSize (V2 osx osy)) =
  LSize $ V2 (floor (fromIntegral osx * sx)) (floor (fromIntegral osy * sy))

transform_LPoint :: (Float,Float) -> LPoint -> LPoint -> LPoint -> LPoint
transform_LPoint (sx, sy) trans anchor point = trans + anchor + r' where
  LPoint (V2 px py) = point - anchor
  r' = LPoint $ V2 (floor (fromIntegral px * sx)) (floor (fromIntegral py * sy))

transform_LBox :: (Float,Float) -> LPoint -> LPoint -> LBox -> LBox
transform_LBox (sx, sy) trans anchor LBox {..} = LBox {
    ul = transform_LPoint (sx, sy) trans anchor ul
    , size = scale_LSize (sx, sy) size
  }
-}

{- this wont work due to inversion roundoff issues
modify_sElt_with_cRelBox :: Bool -> SElt -> CRelBox -> SElt
modify_sElt_with_cRelBox isDo selt CRelBox {..} = let
    -- TODO switch to affine transforms
    -- TODO probably just use lens ;__;
    trans = deltaLBox_translate _cRelBox_box
    anchor = ul _cRelBox_original
    LSize (V2 dx dy) = deltaLBox_resizeBy _cRelBox_box
    -- TODO make sure you don't scale 0 boxes
    LSize (V2 x y) = size _cRelBox_original
    sx :: Float = 1 + fromIntegral dx / fromIntegral x
    sy :: Float = 1 + fromIntegral dy / fromIntegral y
  in
    case selt of
      SEltBox SBox {..}  -> SEltBox $ SBox {
          _sBox_box = traceShowId $ transform_LBox (sx, sy) trans anchor _sBox_box
          , _sBox_style = _sBox_style
        }
      SEltLine SLine {..} -> SEltLine $ SLine {
          _sLine_start = transform_LPoint (sx, sy) trans anchor _sLine_start
          , _sLine_end = transform_LPoint (sx, sy) trans anchor _sLine_end
          , _sLine_style = _sLine_style
        }
      SEltText SText {..} -> SEltText $ SText {
          _sText_box     = transform_LBox (sx, sy) trans anchor _sText_box
          , _sText_text  = _sText_text
          , _sText_style = _sText_style
        }
      x          -> x
-}

modifyDelta :: (Delta x dx) => Bool -> x ->  dx -> x
modifyDelta isDo x dx = if isDo
  then plusDelta x dx
  else minusDelta x dx

updateFnFromController :: Bool -> Controller -> (SEltLabel -> SEltLabel)
updateFnFromController isDo = \case
  (CTagBox :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltBox s -> SEltLabel sname (SEltBox $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagBox - " <> show selt
  (CTagLine :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltLine s -> SEltLabel sname (SEltLine $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagLine - " <> show selt
  (CTagText :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltText s -> SEltLabel sname (SEltText $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagText - " <> show selt
  -- TODO
  --(CTagRelBox :=> Identity d) -> \(SEltLabel sname selt) ->
  --  SEltLabel sname (modify_sElt_with_cRelBox isDo selt d)
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
