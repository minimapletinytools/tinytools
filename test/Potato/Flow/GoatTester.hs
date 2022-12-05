{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Potato.Flow.GoatTester where

import           Relude                            hiding (empty, fromList, first)

import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.DebugHelpers

import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Text                         as T
import           Data.These
import Data.Default


type GoatTesterTrackingState = (Text, Int, Int)

-- TODO add a test name field
data GoatTesterRecord = GoatTesterRecord {
  _goatTesterFailureRecord_trackingState :: GoatTesterTrackingState
  , _goatTesterFailureRecord_failureMessage :: Maybe Text
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
      _goatTesterState_goatState = makeGoatState (emptyOwlPFState, emptyControllerMeta)
      , _goatTesterState_rawOperationCount = 0
      , _goatTesterState_marker = "__START__"
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
      , _goatTesterState_records = []
    }

newtype GoatTesterT m a = GoatTesterT { unGoatTester :: StateT GoatTesterState m a } deriving (Functor, Applicative, Monad, MonadState GoatTesterState)
type GoatTester a = GoatTesterT Identity a


runCommand :: (Monad m) => GoatCmd -> GoatTesterT m ()
runCommand cmd = GoatTesterT $ modify $
  \gts@GoatTesterState {..} -> gts {
      _goatTesterState_goatState = foldGoatFn cmd _goatTesterState_goatState
      , _goatTesterState_rawOperationCount = _goatTesterState_rawOperationCount + 1
      , _goatTesterState_rawOperationCountSinceLastMarker = _goatTesterState_rawOperationCountSinceLastMarker + 1
    }

setMarker :: (Monad m) => Text -> GoatTesterT m ()
setMarker marker = GoatTesterT $ modify $
  \gts -> gts {
      _goatTesterState_marker = marker
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
    }

getTrackingState :: (Monad m) => GoatTesterT m GoatTesterTrackingState
getTrackingState = GoatTesterT $ do
  GoatTesterState {..} <- get
  return $ (_goatTesterState_marker, _goatTesterState_rawOperationCountSinceLastMarker, _goatTesterState_rawOperationCount)

verifyState' :: (Monad m) => (GoatState -> Maybe Text) -> GoatTesterT m Bool
verifyState' fn = GoatTesterT $ do
  gts <- get
  ts <- unGoatTester getTrackingState
  let mf = fn (_goatTesterState_goatState gts)
  let
    record = GoatTesterRecord {
        _goatTesterFailureRecord_trackingState = ts
        , _goatTesterFailureRecord_failureMessage = mf
      }
  put $ gts { _goatTesterState_records = record : _goatTesterState_records gts }
  return $ isJust mf

-- TODO take a test name
verifyState :: (Monad m) => (GoatState -> Maybe Text) -> GoatTesterT m ()
verifyState f = verifyState' f >> return ()

--verifyStateFatal :: (GoatState -> Maybe Text) -> GoatTesterT m ()
--verifyStateFatal = undefined

runGoatTesterT :: (Monad m) => GoatState -> GoatTesterT m a -> m [GoatTesterRecord]
runGoatTesterT gs m = do
  gts <- execStateT (unGoatTester m) $ def { _goatTesterState_goatState = gs }
  return $ reverse $ _goatTesterState_records gts

runGoatTester :: GoatState -> GoatTester a -> [GoatTesterRecord]
runGoatTester gs m = runIdentity $ runGoatTesterT gs m

assertGoatTesterWithOwlPFState :: OwlPFState -> GoatTester a -> Test
assertGoatTesterWithOwlPFState pfs m = do
  let
    rslt = runGoatTester (makeGoatState (pfs, emptyControllerMeta)) m
  TestList $ (flip fmap) rslt $ \GoatTesterRecord {..} -> TestCase $ assertString (maybe "" T.unpack _goatTesterFailureRecord_failureMessage)


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


-- verification helpers

-- | verifies that the number of owls (elts) in the state is what is expected, includes folders in the count
verifyOwlCount :: (Monad m) => Int -> GoatTesterT m ()
verifyOwlCount expected = verifyState f where
  f gs = if count == expected
    then Nothing
    else Just $ "verifyOwlCount failed: got " <> show count <> " owls, expected " <> show expected
    where count = IM.size . _owlTree_mapping . hasOwlTree_owlTree . goatState_pFState $ gs

maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy' f l = if length l == 0
  then Nothing
  else Just $ L.maximumBy f l

verifyMostRecentlyCreatedOwl :: (Monad m) => (SuperOwl -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedOwl f = verifyState f' where
  f' gs = r where
    msowl = maximumBy' (\s1 s2 -> compare (_superOwl_id s1) (_superOwl_id s2)) $ owliterateall (hasOwlTree_owlTree $ goatState_pFState gs)
    r = case msowl of
      Nothing -> Just "verifyMostRecentlyCreatedOwl failed, no 🦉s"
      Just sowl -> (\m -> "verifyMostRecentlyCreatedOwl failed with message: " <> m <> "\ngot:\n" <> potatoShow (_superOwl_elt sowl)) <$> f sowl

verifyMostRecentlyCreatedLine :: (Monad m) => (SAutoLine -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedLine f = verifyMostRecentlyCreatedOwl f' where
  f' sowl = case _owlItem_subItem (_superOwl_elt sowl) of
    OwlSubItemLine sline _ -> fmap ("verifyMostRecentlyCreatedLine failed, " <>) $ f sline
    x -> Just $ "verifyMostRecentlyCreatedLine failed, expected SAutoLine got: " <> show x

-- otheruseful stuff

-- export as part of this module becaues it's super useful
-- from https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Data.Maybe.HT.html#toMaybe
{-# INLINE toMaybe #-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x
