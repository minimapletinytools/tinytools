{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Common
  (
  GoatWidgetCmd(..)
  , everything_network_app
  , EverythingPredicate(..)
  , testEverythingPredicate
  , showEverythingPredicate
  , checkNumElts
  , checkHandlerName
  , checkHandlerNameAndState
  , numEltsInLBoxUsingBroadphasePredicate
  , numSelectedEltsEqualPredicate
  , firstSelectedSuperOwlPredicate
  , firstSuperOwlPredicate
  , constructTest
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.Controller.GoatWidget
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Types
import           Potato.Flow.Deprecated.TestStates

import           Control.Monad.Fix
import qualified Data.IntMap                       as IM
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import           Data.These


-- generic testing
data GoatWidgetCmd =
  EWCMouse LMouseData
  | EWCKeyboard KeyboardData
  | EWCTool Tool
  | EWCSetParams ControllersWithId
  | EWCLoad (SPotatoFlow, ControllerMeta)
  | EWCNothing
  | EWCLabel Text
  | EWCCanvasResize XY

everything_network_app
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => OwlPFState
  -> AppIn t () GoatWidgetCmd -> TestGuestT t m (AppOut t GoatState GoatState)
everything_network_app pfs (AppIn _ ev) = do
  let ewc = GoatWidgetConfig  {
      _goatWidgetConfig_initialState = pfs
      , _goatWidgetConfig_load = fforMaybe ev $ \case
        EWCLoad x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_setDebugLabel = fforMaybe ev $ \case
        EWCLabel x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_mouse = fforMaybe ev $ \case
        EWCMouse x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_keyboard = fforMaybe ev $ \case
        EWCKeyboard x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_selectTool = fforMaybe ev $ \case
        EWCTool x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_paramsEvent = fforMaybe ev $ \case
        EWCSetParams x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_canvasSize = fforMaybe ev $ \case
        EWCCanvasResize x -> Just x
        _ -> Nothing
      , _goatWidgetConfig_bypassEvent = never

    }
  everythingWidget <- holdGoatWidget ewc
  let rDyn = _goatWidget_DEBUG_goatState everythingWidget
  return $ AppOut (current rDyn) (updated rDyn)


data EverythingPredicate where
  EqPredicate :: (Show a, Eq a) => (GoatState -> a) -> a -> EverythingPredicate
  FunctionPredicate :: (GoatState -> (Text, Bool)) -> EverythingPredicate
  PFStateFunctionPredicate :: (OwlPFState -> (Text, Bool)) -> EverythingPredicate
  AlwaysPass :: EverythingPredicate
  LabelCheck :: Text -> EverythingPredicate
  Combine :: [EverythingPredicate] -> EverythingPredicate

testEverythingPredicate :: EverythingPredicate -> GoatState -> Bool
testEverythingPredicate (EqPredicate f a) e     = f e == a
testEverythingPredicate (FunctionPredicate f) e = snd $ f e
testEverythingPredicate (PFStateFunctionPredicate f) e = snd . f $ goatState_pFState e
testEverythingPredicate AlwaysPass _            = True
-- TODO actually set a label and pipe it through EverythingCombined_DEBUG?
testEverythingPredicate (LabelCheck l) e = l == _goatState_debugLabel e
testEverythingPredicate (Combine xs) e          = all id . map (\p -> testEverythingPredicate p e) $ xs

showEverythingPredicate :: EverythingPredicate -> GoatState -> Text
showEverythingPredicate (EqPredicate f a) e = "expected: " <> show a <> " got: " <> show (f e)
showEverythingPredicate (FunctionPredicate f) e = fst $ f e
showEverythingPredicate (PFStateFunctionPredicate f) e = fst . f $ goatState_pFState e
showEverythingPredicate AlwaysPass _ = "always pass"
-- TODO actually set a label and pipe it through EverythingCombined_DEBUG?
showEverythingPredicate (LabelCheck l) e = "expected label: " <> show l <> " got: " <> show (_goatState_debugLabel e)
-- TODO this should not print passing tests as well :(
showEverythingPredicate (Combine xs) e = "[" <> foldr (\p acc -> showEverythingPredicate p e <> ", " <> acc) "" xs <> "]"

checkNumElts :: Int -> OwlPFState -> (Text, Bool)
checkNumElts n OwlPFState {..} = (t,r) where
  ds = IM.size (_owlTree_mapping _owlPFState_owlTree)
  r = ds == n
  t = "expected: " <> show n <> " owlTree: " <> show ds

numEltsInLBoxUsingBroadphasePredicate :: Int -> LBox -> EverythingPredicate
numEltsInLBoxUsingBroadphasePredicate n lbox = FunctionPredicate $
  (\(BroadPhaseState bps) ->
    let gotn = length $ broadPhase_cull lbox bps
    in ("BroadPhase passed: " <> show gotn <> " expected: " <> show n <> " broadphase: " <> show bps, gotn == n))
  . _goatState_broadPhaseState

numSelectedEltsEqualPredicate :: Int -> EverythingPredicate
numSelectedEltsEqualPredicate n = FunctionPredicate $
  (\s ->
    let nSelected = Seq.length s
    in ("Selected: " <> show nSelected <> " expected: " <> show n, nSelected == n))
  . unSuperOwlParliament . _goatState_selection

-- you can prob delete this, better to always check for state using version below
checkHandlerName :: Text -> EverythingPredicate
checkHandlerName name = FunctionPredicate $
  (\(SomePotatoHandler h) ->
    let hName = pHandlerName h
    in ("Handler: " <> hName <> " expected: " <> name, hName == name))
  . _goatState_handler

checkHandlerNameAndState :: Text -> Bool -> EverythingPredicate
checkHandlerNameAndState name state = FunctionPredicate $
  (\(SomePotatoHandler h) ->
    let
      hName = pHandlerName h
      hState = pIsHandlerActive h
    in ("Handler: " <> hName <> "(" <> show hState <> ") expected: " <> name <> " (" <> show state <> ")", hName == name && hState == state))
  . _goatState_handler

firstSelectedSuperOwlPredicate :: Maybe Text -> (SuperOwl -> Bool) -> EverythingPredicate
firstSelectedSuperOwlPredicate mlabel f = FunctionPredicate $
  (\s ->
    let
      mfirst = case mlabel of
        Nothing    -> Seq.lookup 0 s
        Just label -> find (\sowl -> isOwl_name sowl == label) s
    in case mfirst of
      Nothing    -> ("No elt with label " <> show mlabel <> show s, False)
      Just first -> ("First selected: " <> show first, f first))
  . unSuperOwlParliament . _goatState_selection

firstSuperOwlPredicate :: Maybe Text -> (SuperOwl -> Bool) -> EverythingPredicate
firstSuperOwlPredicate mlabel f = FunctionPredicate $
  (\ot ->
    let
      sowls = toList $ owliterateall ot
      mfirst = case mlabel of
        Nothing -> case sowls of
          []  -> Nothing
          x:_ -> Just x
        Just label -> case find (\sowl -> isOwl_name sowl == label) sowls of
          Nothing     -> Nothing
          Just sowl -> Just sowl
    in case mfirst of
      Nothing    -> ("No elt with label " <> show mlabel, False)
      Just first -> ("First elt with label " <> show mlabel <> ": " <> show first, f first))
  . _owlPFState_owlTree . goatState_pFState


constructTest :: String -> OwlPFState -> [GoatWidgetCmd] -> [EverythingPredicate] -> Test
constructTest label pfs bs expected = TestLabel label $ TestCase $ do
  let
    run = runApp (everything_network_app pfs) () (fmap (Just . That) bs)
  values :: [[(GoatState, Maybe GoatState)]]
    <- liftIO run

  -- expect only 1 tick per event
  forM values $ \x -> assertEqual "expect only 1 tick per event" (length x) 1

  -- expect correct number of outputs
  assertEqual "expect correct number of outputs" (length values) (length expected)

  -- TODO do index counting from labels
  -- expect each output matches predicate
  -- if no output event, uses output behavior to test predicate
  forM_ (zip3 (join values) expected [0..]) $ \((b, me), p, i) -> let
      testfn ewcd =
        (assertBool . T.unpack) ((showEverythingPredicate p ewcd)
          <> " \n[label = " <> _goatState_debugLabel ewcd
          <> ", index = " <> show i <> "]"
          <> "\nstate = " <> case me of
            Just e  -> show (goatState_pFState e)
            Nothing -> show (goatState_pFState b))
        (testEverythingPredicate p ewcd)
    in case me of
      Just e  -> testfn e
      Nothing -> testfn b
