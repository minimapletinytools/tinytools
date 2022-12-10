{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Potato.Flow.GoatTester where

import           Relude                            hiding (empty, fromList, first)

import           Test.Hspec

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.DebugHelpers

import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Text                         as T
import           Data.These
import Data.Default
import Data.Tuple.Extra


type GoatTesterTrackingState = (Text, Int, Int) -- (marker, operations since start of test, operations since last marker)

data GoatTesterRecord = GoatTesterRecord {
  _goatTesterRecord_trackingState :: GoatTesterTrackingState
  , _goatTesterRecord_failureMessage :: Maybe Text
  , _goatTesterRecord_description :: Text
} deriving (Show)

data GoatTesterState = GoatTesterState {
    _goatTesterState_goatState :: GoatState
    , _goatTesterState_rawOperationCount :: Int

    -- TODO consider making marker more complex
    , _goatTesterState_marker :: Text

    , _goatTesterState_rawOperationCountSinceLastMarker :: Int
    , _goatTesterState_records :: [GoatTesterRecord]
  } deriving (Show)

instance Default GoatTesterState where
  def = GoatTesterState {
      _goatTesterState_goatState = makeGoatState (V2 100 100) (emptyOwlPFState, emptyControllerMeta)
      , _goatTesterState_rawOperationCount = 0
      , _goatTesterState_marker = "__START__"
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
      , _goatTesterState_records = []
    }

newtype GoatTesterT m a = GoatTesterT { unGoatTesterT :: StateT GoatTesterState m a } deriving (Functor, Applicative, Monad, MonadState GoatTesterState)
type GoatTester a = GoatTesterT Identity a


runCommand :: (Monad m) => GoatCmd -> GoatTesterT m ()
runCommand cmd = GoatTesterT $ modify $
  \gts@GoatTesterState {..} -> gts {
      _goatTesterState_goatState = foldGoatFn cmd _goatTesterState_goatState
      , _goatTesterState_rawOperationCount = _goatTesterState_rawOperationCount + 1
      , _goatTesterState_rawOperationCountSinceLastMarker = _goatTesterState_rawOperationCountSinceLastMarker + 1
    }



getTrackingState :: (Monad m) => GoatTesterT m GoatTesterTrackingState
getTrackingState = GoatTesterT $ do
  GoatTesterState {..} <- get
  return $ (_goatTesterState_marker, _goatTesterState_rawOperationCountSinceLastMarker, _goatTesterState_rawOperationCount)

putRecord :: (Monad m) => GoatTesterRecord -> GoatTesterT m ()
putRecord record = GoatTesterT $ do
  gts <- get
  put $ gts { _goatTesterState_records = record : _goatTesterState_records gts }

setMarker :: (Monad m) => Text -> GoatTesterT m ()
setMarker marker = GoatTesterT $ do
  modify $ \gts -> gts {
      _goatTesterState_marker = marker
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
    }

verifyState' :: (Monad m) => Text -> (GoatState -> Maybe Text) -> GoatTesterT m Bool
verifyState' desc fn = GoatTesterT $ do
  gts <- get
  ts <- unGoatTesterT getTrackingState
  let mf = fn (_goatTesterState_goatState gts)
  unGoatTesterT $ putRecord $ GoatTesterRecord {
      _goatTesterRecord_trackingState = ts
      , _goatTesterRecord_failureMessage = mf
      , _goatTesterRecord_description = desc
    }
  return $ isJust mf

-- TODO take a test name
verifyState :: (Monad m) => Text -> (GoatState -> Maybe Text) -> GoatTesterT m ()
verifyState desc f = verifyState' desc f >> return ()

--verifyStateFatal :: (GoatState -> Maybe Text) -> GoatTesterT m ()
--verifyStateFatal = undefined

runGoatTesterT :: (Monad m) => GoatState -> GoatTesterT m a -> m [GoatTesterRecord]
runGoatTesterT gs m = do
  gts <- execStateT (unGoatTesterT m) $ def { _goatTesterState_goatState = gs }
  return $ reverse $ _goatTesterState_records gts

runGoatTester :: GoatState -> GoatTester a -> [GoatTesterRecord]
runGoatTester gs m = runIdentity $ runGoatTesterT gs m

hSpecGoatTesterWithOwlPFState :: OwlPFState -> GoatTester a -> SpecWith ()
hSpecGoatTesterWithOwlPFState pfs m = do
  let
    rslt' = runGoatTester (makeGoatState (V2 100 100) (pfs, emptyControllerMeta)) m
    rslt = L.groupBy (\a b -> fst3 (_goatTesterRecord_trackingState a) == fst3 (_goatTesterRecord_trackingState b)) rslt'
  forM_ rslt $ \gtss -> case gtss of
    [] -> return ()
    (x:xs) -> do
      describe (T.unpack (fst3 (_goatTesterRecord_trackingState x))) $ forM_ (x:xs) $ \GoatTesterRecord {..} -> do
        it (T.unpack _goatTesterRecord_description) $ case _goatTesterRecord_failureMessage of
          Nothing -> return ()
          Just x -> expectationFailure (T.unpack x)


-- operation helpers

setTool :: (Monad m) => Tool -> GoatTesterT m ()
setTool tool = runCommand $ GoatCmdTool tool

canvasMouseToScreenMouse :: GoatState -> XY -> XY
canvasMouseToScreenMouse gs pos = owlPFState_fromCanvasCoordinates (goatState_pFState gs) pos + _goatState_pan gs

mouse :: (Monad m) => Bool -> Bool -> [KeyModifier] -> MouseButton -> XY -> GoatTesterT m ()
mouse isCanvas isRelease modifiers button pos = do
  gts <- get
  runCommand $ GoatCmdMouse $ LMouseData {
    _lMouseData_position       = if isCanvas then canvasMouseToScreenMouse (_goatTesterState_goatState gts ) pos else pos
    , _lMouseData_isRelease    = isRelease
    , _lMouseData_button       = button
    , _lMouseData_modifiers    = modifiers
    , _lMouseData_isLayerMouse = not isCanvas
  }

canvasMouseDown :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
canvasMouseDown (x,y) = mouse True False [] MouseButton_Left (V2 x y)

canvasMouseUp :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
canvasMouseUp (x,y) = mouse True True [] MouseButton_Left (V2 x y)

pressKey :: (Monad m) => KeyboardKey -> GoatTesterT m ()
pressKey k = runCommand (GoatCmdKeyboard (KeyboardData k []))

pressKeys :: (Monad m) => String -> GoatTesterT m ()
pressKeys text = forM_ text $ \c -> pressKey (KeyboardKey_Char c)

pressEscape :: (Monad m) => GoatTesterT m ()
pressEscape = pressKey KeyboardKey_Esc

pressDelete :: (Monad m) => GoatTesterT m ()
pressDelete = pressKey KeyboardKey_Delete

pressUndo :: (Monad m) => GoatTesterT m ()
pressUndo = runCommand (GoatCmdKeyboard (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl]))

pressRedo :: (Monad m) => GoatTesterT m ()
pressRedo = runCommand (GoatCmdKeyboard (KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl]))

-- verification helpers

-- | verifies that the number of owls (elts) in the state is what is expected, includes folders in the count
verifyOwlCount :: (Monad m) => Int -> GoatTesterT m ()
verifyOwlCount expected = verifyState "verifyOwlCount" f where
  f gs = if count == expected
    then Nothing
    else Just $ "got " <> show count <> " owls, expected " <> show expected
    where count = IM.size . _owlTree_mapping . hasOwlTree_owlTree . goatState_pFState $ gs

maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy' f l = if length l == 0
  then Nothing
  else Just $ L.maximumBy f l

verifyMostRecentlyCreatedOwl' :: (Monad m) => Text -> (SuperOwl -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedOwl' desc f = verifyState desc f' where
  f' gs = r where
    msowl = maximumBy' (\s1 s2 -> compare (_superOwl_id s1) (_superOwl_id s2)) $ owliterateall (hasOwlTree_owlTree $ goatState_pFState gs)
    r = case msowl of
      Nothing -> Just "failed, no 🦉s"
      Just sowl -> (\m -> "failed with message: " <> m <> "\ngot:\n" <> potatoShow (_superOwl_elt sowl)) <$> f sowl

verifyMostRecentlyCreatedOwl :: (Monad m) => (SuperOwl -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedOwl = verifyMostRecentlyCreatedOwl' "verifyMostRecentlyCreatedOwl"

verifyMostRecentlyCreatedLine :: (Monad m) => (SAutoLine -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedLine f = verifyMostRecentlyCreatedOwl' "verifyMostRecentlyCreatedLine" f' where
  f' sowl = case _owlItem_subItem (_superOwl_elt sowl) of
    OwlSubItemLine sline _ -> f sline
    x -> Just $ "expected SAutoLine got: " <> show x

-- otheruseful stuff

-- export as part of this module becaues it's super useful
-- from https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Data.Maybe.HT.html#toMaybe
{-# INLINE toMaybe #-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

alwaysFail :: (Monad m) => Text -> GoatTesterT m ()
alwaysFail msg = GoatTesterT $ do
  gts <- get
  ts <- unGoatTesterT getTrackingState
  unGoatTesterT $ putRecord $ GoatTesterRecord {
      _goatTesterRecord_trackingState = ts
      , _goatTesterRecord_failureMessage = Just msg
      , _goatTesterRecord_description = "this test always fails"
    }
