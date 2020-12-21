{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Common
  (
  EverythingWidgetCmd(..)
  , everything_network_app
  , EverythingPredicate(..)
  , testEverythingPredicate
  , showEverythingPredicate
  , checkNumElts
  , checkHandlerName
  , checkHandlerNameAndState
  , numSelectedEltsEqualPredicate
  , firstSelectedSuperSEltLabelPredicate
  , firstSuperSEltLabelPredicate
  , constructTest
  )
where

import           Relude                                  hiding (empty,
                                                          fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.Controller.Everything
import           Potato.Flow.Controller.EverythingWidget
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.TestStates

import           Control.Monad.Fix
import qualified Data.IntMap                             as IM
import qualified Data.Sequence                           as Seq
import qualified Data.Text                               as T
import           Data.These


-- generic testing
data EverythingWidgetCmd =
  EWCMouse LMouseData
  | EWCKeyboard KeyboardData
  | EWCTool Tool
  | EWCSelect Selection Bool -- true if add to selection
  | EWCLoad (PFState, ControllerMeta)
  | EWCNothing
  | EWCLabel Text

everything_network_app
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => PFState
  -> AppIn t () EverythingWidgetCmd -> TestGuestT t m (AppOut t EverythingCombined_DEBUG EverythingCombined_DEBUG)
everything_network_app pfs (AppIn _ ev) = do
  let ewc = EverythingWidgetConfig  {
      _everythingWidgetConfig_initialState = pfs
      , _everythingWidgetConfig_load = fforMaybe ev $ \case
        EWCLoad x -> Just x
        _ -> Nothing
      , _everythingWidgetConfig_setDebugLabel = fforMaybe ev $ \case
        EWCLabel x -> Just x
        _ -> Nothing
      , _everythingWidgetConfig_mouse = fforMaybe ev $ \case
        EWCMouse x -> Just x
        _ -> Nothing
      , _everythingWidgetConfig_keyboard = fforMaybe ev $ \case
        EWCKeyboard x -> Just x
        _ -> Nothing
      , _everythingWidgetConfig_selectTool = fforMaybe ev $ \case
        EWCTool x -> Just x
        _ -> Nothing
      , _everythingWidgetConfig_selectNew = fforMaybe ev $ \case
        EWCSelect x False -> Just x
        _ -> Nothing
      , _everythingWidgetConfig_selectAdd = fforMaybe ev $ \case
        EWCSelect x True -> Just x
        _ -> Nothing
    }
  everythingWidget <- holdEverythingWidget ewc
  let rDyn = _everythingWidget_everythingCombined_DEBUG everythingWidget
  return $ AppOut (current rDyn) (updated rDyn)


data EverythingPredicate where
  EqPredicate :: (Show a, Eq a) => (EverythingCombined_DEBUG -> a) -> a -> EverythingPredicate
  FunctionPredicate :: (EverythingCombined_DEBUG -> (Text, Bool)) -> EverythingPredicate
  PFStateFunctionPredicate :: (PFState -> (Text, Bool)) -> EverythingPredicate
  AlwaysPass :: EverythingPredicate
  LabelCheck :: Text -> EverythingPredicate
  Combine :: [EverythingPredicate] -> EverythingPredicate

testEverythingPredicate :: EverythingPredicate -> EverythingCombined_DEBUG -> Bool
testEverythingPredicate (EqPredicate f a) e     = f e == a
testEverythingPredicate (FunctionPredicate f) e = snd $ f e
testEverythingPredicate (PFStateFunctionPredicate f) e = snd . f $ _everythingCombined_pFState e
testEverythingPredicate AlwaysPass _            = True
-- TODO actually set a label and pipe it through EverythingCombined_DEBUG?
testEverythingPredicate (LabelCheck l) e = l == _everythingCombined_debugLabel e
testEverythingPredicate (Combine xs) e          = all id . map (\p -> testEverythingPredicate p e) $ xs

showEverythingPredicate :: EverythingPredicate -> EverythingCombined_DEBUG -> Text
showEverythingPredicate (EqPredicate f a) e = "expected: " <> show a <> " got: " <> show (f e)
showEverythingPredicate (FunctionPredicate f) e = fst $ f e
showEverythingPredicate (PFStateFunctionPredicate f) e = fst . f $ _everythingCombined_pFState e
showEverythingPredicate AlwaysPass _ = "always pass"
-- TODO actually set a label and pipe it through EverythingCombined_DEBUG?
showEverythingPredicate (LabelCheck l) e = "expected label: " <> show l <> " got: " <> show (_everythingCombined_debugLabel e)
showEverythingPredicate (Combine xs) e = "[" <> foldr (\p acc -> showEverythingPredicate p e <> ", " <> acc) "" xs <> "]"

checkNumElts :: Int -> PFState -> (Text, Bool)
checkNumElts n PFState {..} = (t,r) where
  ds = IM.size _pFState_directory
  ls = Seq.length _pFState_layers
  r = ds == n && ls == n
  t = "expected: " <> show n <> " dir: " <> show ds <> " layers: " <> show ls

numSelectedEltsEqualPredicate :: Int -> EverythingPredicate
numSelectedEltsEqualPredicate n = FunctionPredicate $
  (\s ->
    let nSelected = Seq.length s
    in ("Selected: " <> show nSelected <> " expected: " <> show n, nSelected == n))
  . _everythingCombined_selection

-- you can prob delete this, better to always check for state using version below
checkHandlerName :: Text -> EverythingPredicate
checkHandlerName name = FunctionPredicate $
  (\(SomePotatoHandler h) ->
    let hName = pHandlerName h
    in ("Handler: " <> hName <> " expected: " <> name, hName == name))
  . _everythingCombined_handler

checkHandlerNameAndState :: Text -> Bool -> EverythingPredicate
checkHandlerNameAndState name state = FunctionPredicate $
  (\(SomePotatoHandler h) ->
    let
      hName = pHandlerName h
      hState = pIsHandlerActive h
    in ("Handler: " <> hName <> "(" <> show hState <> ") expected: " <> name <> " (" <> show state <> ")", hName == name && hState == state))
  . _everythingCombined_handler

firstSelectedSuperSEltLabelPredicate :: Maybe Text -> (SuperSEltLabel -> Bool) -> EverythingPredicate
firstSelectedSuperSEltLabelPredicate mlabel f = FunctionPredicate $
  (\s ->
    let
      mfirst = case mlabel of
        Nothing    -> Seq.lookup 0 s
        Just label -> find (\(_,_,SEltLabel l _) -> label == l) s
    in case mfirst of
      Nothing    -> ("No elt with label " <> show mlabel <> show s, False)
      Just first -> ("First selected: " <> show first, f first))
  . _everythingCombined_selection

firstSuperSEltLabelPredicate :: Maybe Text -> (SuperSEltLabel -> Bool) -> EverythingPredicate
firstSuperSEltLabelPredicate mlabel f = FunctionPredicate $
  (\sseltls ->
    let
      mfirst = case mlabel of
        Nothing -> case toList sseltls of
          []  -> Nothing
          x:_ -> Just x
        Just label -> case find (\(_, _, SEltLabel l _) -> label == l) sseltls of
          Nothing     -> Nothing
          Just sseltl -> Just sseltl
    in case mfirst of
      Nothing    -> ("No elt with label " <> show mlabel, False)
      Just first -> ("First elt with label " <> show mlabel <> ": " <> show first, f first))
  . pFState_to_superSEltLabelSeq . _everythingCombined_pFState


constructTest :: String -> PFState -> [EverythingWidgetCmd] -> [EverythingPredicate] -> Test
constructTest label pfs bs expected = TestLabel label $ TestCase $ do
  let
    run = runApp (everything_network_app pfs) () (fmap (Just . That) bs)
  values :: [[(EverythingCombined_DEBUG, Maybe EverythingCombined_DEBUG)]]
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
          <> " \n[label = " <> _everythingCombined_debugLabel ewcd
          <> ", index = " <> show i <> "]"
          <> "\nstate = " <> case me of
            Just e  -> show (_everythingCombined_pFState e)
            Nothing -> show (_everythingCombined_pFState b))
        (testEverythingPredicate p ewcd)
    in case me of
      Just e  -> testfn e
      Nothing -> testfn b
