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
import           Data.These

someState1 :: PFState
someState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, someSEltLabel), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel), (5, folderEnd)]
      , _pFState_canvas = someSCanvas
  }


pfoWithInitialState :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m) => PFState -> m (PFOutput t)
pfoWithInitialState pfState = holdPFWithInitialState pfState neverPFConfig

-- bespoke testing
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
  pfo <- pfoWithInitialState someState1
  let
    addSelectEv = fmapMaybe (\(b,s) -> if b then Just s else Nothing) ev
    newSelectEv = fmapMaybe (\(b,s) -> if not b then Just s else Nothing) ev
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig {
      _everythingWidgetConfig_potatoFlow = pfo
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
-- TODO figure out input event type
everything_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => AppIn t () () -> TestGuestT t m (AppOut t (EverythingWidget t) ())
everything_network ev = do
  pfo <- pfoWithInitialState someState1
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig {
      -- TODO
      _everythingWidgetConfig_potatoFlow = pfo
    }
  return $ AppOut (constant everythingWidget) never

everything_basic_test :: Test
everything_basic_test = TestLabel "everything_basic" $ TestCase $ do
  -- TODO test something
  let
    bs = [()]
    expected = [Nothing]
    run = runApp everything_network () (fmap (Just . That) bs)
  v <- liftIO run
  let
    eventsOnly :: [[Maybe ()]]
    eventsOnly = fmap (fmap snd) v
  (join eventsOnly) @?= expected




spec :: Spec
spec = do
  describe "EverythingWidget" $ do
    fromHUnitTest $ tool_test
    fromHUnitTest $ select_test
    fromHUnitTest $ everything_basic_test
