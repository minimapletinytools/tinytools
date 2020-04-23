{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Reflex.RElts (
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Manipulators
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Monad.Fix

import           Data.Dependent.Sum              (DSum ((:=>)), (==>))
import qualified Data.Dependent.Sum              as DS
import           Data.Functor.Misc
import qualified Data.IntMap.Strict              as IM
import           Data.Maybe                      (fromJust)

import           Reflex
import qualified Reflex.Patch.IntMap             as IM
import           Reflex.Potato.Helpers


{-
type ManipulatorWithId t = DS.DSum (Const2 LayerEltId (Manipulators t)) Identity
type ControllerWithId = DS.DSum (Const2 LayerEltId Controller) Identity
type ControllerEventSelector t = EventSelector t (Const2 LayerEltId Controller)
-}


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

type Selected = [(REltId, SEltLabel)]
type Manipulating t = ([REltId], Manipulator t)
makeManipulators :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Dynamic t Selected
  -> m (Dynamic t (Manipulating t))
makeManipulators selected = do
  let
    nilState :: Manipulating t
    nilState = ([], MTagNone ==> ())
    foldfn :: Selected -> Manipulating t -> PushM t (Manipulating t)
    foldfn [] _ = return nilState
    foldfn ((rid, SEltLabel _ selt):[]) _ = case selt of
      -- TODO
      SEltBox SBox {..} -> undefined
      _                 -> undefined
    foldfn selected _ = do
      let
        -- TODO
        sboxes = map (getSEltBox . selt_elt . snd) selected
        combined = undefined
      return (map fst selected, MTagRelBox ==> combined)
  foldDynM foldfn nilState (updated selected)



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
          (CTagBox :=> Identity dbox) -> plusDelta box dbox
        foldfn (Right ct) box = case ct of
          (CTagBox :=> Identity dbox) -> minusDelta box dbox
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
