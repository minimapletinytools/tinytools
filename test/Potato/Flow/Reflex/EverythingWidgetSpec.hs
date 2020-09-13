{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.EverythingWidgetSpec
  ( spec
  )
where

import           Relude                              hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit            (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.Reflex.Everything
import           Potato.Flow.Reflex.EverythingWidget
import           Potato.Flow.TestStates

import           Control.Monad.Fix
import qualified Data.IntMap                         as IM
import qualified Data.Sequence                       as Seq

someState1 :: PFState
someState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, someSEltLabel), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel), (5, folderEnd)]
      , _pFState_canvas = someSCanvas
  }

-- simple bespoke testing
tool_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Tool -> TestGuestT t m (Event t Tool))
tool_network ev = do
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig { _everythingWidgetConfig_selectTool = ev }
  return $ updated . _everythingWidget_tool $ everythingWidget

tool_test :: Test
tool_test = TestLabel "tool" $ TestCase $ do
  let
    -- note, starting value is TSelect
    bs = [Tool_Pan, Tool_Select, Tool_Pan, Tool_Pan, Tool_Box, Tool_Line, Tool_Text]
    expected = [Just Tool_Pan, Just Tool_Select, Just Tool_Pan, Nothing, Just Tool_Box, Just Tool_Line, Just Tool_Text]
    run = runAppSimple tool_network bs
  v <- liftIO run
  (join v) @?= expected

select_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t (Bool, Selection) -> TestGuestT t m (Event t Selection)
select_network ev = do
  let
    addSelectEv = fmapMaybe (\(b,s) -> if b then Just s else Nothing) ev
    newSelectEv = fmapMaybe (\(b,s) -> if not b then Just s else Nothing) ev
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig {
      _everythingWidgetConfig_initialState = someState1
      , _everythingWidgetConfig_selectNew = newSelectEv
      , _everythingWidgetConfig_selectAdd = addSelectEv
    }
  return $ updated . _everythingWidget_selection $ everythingWidget

select_test :: Test
select_test = TestLabel "select" $ TestCase $ do
  let
    -- TODO need to set some initial state with elements in order to test something meaningful here
    -- adding Seq.empty means no change in selection so no output event
    bs = [(False, Seq.empty)]
    expected = [Nothing]
    run = runAppSimple select_network bs
  v <- liftIO run
  (join v) @?= expected

-- generic testing
data EverythingWidgetCmd =
  EWCMouse LMouseData
  | EWCKeyboard KeyboardData
  | EWCTool Tool
  | EWCSelect Selection Bool -- true if add to selection
  | EWCNothing

everything_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t EverythingWidgetCmd -> TestGuestT t m (Event t EverythingCombined_DEBUG)
everything_network ev = do
  let ewc = EverythingWidgetConfig  {
      _everythingWidgetConfig_initialState = someState1

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
  return $ updated $ _everythingWidget_everythingCombined_DEBUG everythingWidget

data EverythingPredicate where
  EqPredicate :: (Show a, Eq a) => (EverythingCombined_DEBUG -> a) -> a -> EverythingPredicate

testEverythingPredicate :: EverythingPredicate -> EverythingCombined_DEBUG -> Bool
testEverythingPredicate (EqPredicate f a) e = f e == a

showEverythingPredicate :: EverythingPredicate -> EverythingCombined_DEBUG -> String
showEverythingPredicate (EqPredicate f a) e = "expected: " <> show a <> " got: " <> show (f e)

everything_basic_test :: Test
everything_basic_test = TestLabel "everything_basic" $ TestCase $ do
  -- TODO test something
  let
    bs = [
        EWCNothing

        -- test basic panning
        , EWCTool Tool_Pan
        , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left)
        , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left)
      ]

    expected = [
        Nothing
        , Just (EqPredicate _everythingCombined_selectedTool Tool_Pan)
        , Just (EqPredicate _everythingCombined_pan (V2 0 0))
        , Just (EqPredicate _everythingCombined_pan (V2 1 1))
      ]
    run = runAppSimple everything_network bs
  values <- liftIO run

  -- TODO move stuff below into helper function
  -- expect only 1 tick per event
  forM values $ \x -> length x @?= 1
  -- expect correct number of outputs
  length values @?= length expected
  forM_ (zip (join values) expected) $ \(me, p) -> case p of
    Nothing -> assertBool "expected no output" (isNothing me)
    Just p  -> case me of
      Nothing -> assertFailure "expected output"
      Just e  -> assertBool (showEverythingPredicate p e) (testEverythingPredicate p e)




spec :: Spec
spec = do
  describe "EverythingWidget" $ do
    fromHUnitTest $ tool_test
    fromHUnitTest $ select_test
    fromHUnitTest $ everything_basic_test
