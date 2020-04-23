{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Reflex.RElts (
) where

import           Relude
import           Relude.Extra.Foldable1   (foldl1')

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Monad.Fix

import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import qualified Data.Dependent.Sum       as DS
import           Data.Functor.Misc
import qualified Data.IntMap.Strict       as IM
import           Data.Maybe               (fromJust)
import           Data.Tuple.Extra         (fst3, snd3, thd3)

import           Reflex
import qualified Reflex.Patch.IntMap      as IM
import           Reflex.Potato.Helpers


-- | gets an 'LBox' that contains the entire RElt
getSEltBox :: SElt -> Maybe LBox
getSEltBox selt = case selt of
  SEltNone        -> Nothing
  SEltFolderStart -> Nothing
  SEltFolderEnd   -> Nothing
  SEltBox x       -> Just $ sb_box x
  SEltLine x      -> Just $ make_LBox_from_LPoints (sl_start x) (sl_end x)
  SEltText x      -> Just $ st_box x

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
  SEltBox x       -> potatoDrawer
  SEltLine x      -> potatoDrawer
  SEltText x      -> potatoDrawer
  where
    potatoDrawer = SEltDrawer {
        _sEltDrawer_box = fromJust (getSEltBox selt)
        , _sEltDrawer_renderer =  makePotatoRenderer $ fromJust (getSEltBox selt)
      }

type Selected = [SuperSEltLabel]
makeManipulators :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Event t Selected -- ^ selection event, which will sample manipulators of current selection
  -> m (Dynamic t Manipulator)
makeManipulators selected = do
  let
    nilState :: Manipulator
    nilState = (MTagNone ==> ())
    foldfn :: Selected -> Manipulator -> Manipulator
    foldfn [] _ = nilState
    foldfn ((rid, _, SEltLabel _ selt):[]) _ = case selt of
      SEltBox SBox {..} -> (MTagBox ==> mbox) where
        mbox = MBox {
            _mBox_target = rid
            , _mBox_box  = sb_box
          }
      -- TODO rest of them
      _                 -> undefined
    foldfn (s:ss) _ = r where
      nes = s:|ss
      msboxes = catMaybes . toList $ fmap (getSEltBox . selt_elt . thd3) nes
      r = fromMaybe nilState $ flip viaNonEmpty msboxes $ \sboxes ->
        MTagRelBox ==>
          MRelBox {
            _mRelBox_targets    = fmap fst3 nes
            , _mRelBox_box      = foldl1' union_LBox sboxes
          }
  foldDyn foldfn nilState selected


-- TODO DELETE

{-
-- needed by 'makeManipulators' internally

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
      mbox <- foldDyn foldfn sb_box bothEv
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
