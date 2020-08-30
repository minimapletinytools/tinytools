{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.EverythingSpec
  ( spec
  )
where

import           Relude                        hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.Reflex.Everything

import           Control.Monad.Fix
import qualified Data.IntMap                   as IM
import qualified Data.Sequence                 as Seq


-- TODO probably create TestStates.hs and put this stuff in there
folderStart :: SEltLabel
folderStart = SEltLabel "folder" SEltFolderStart

folderEnd :: SEltLabel
folderEnd = SEltLabel "folder (end)" SEltFolderEnd

someSEltLabel :: SEltLabel
someSEltLabel = SEltLabel "some elt" SEltNone

defaultCanvasLBox :: LBox
defaultCanvasLBox = LBox (V2 0 0) (V2 100 50)

someState1 :: PFState
someState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, someSEltLabel), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel), (5, folderEnd)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }


pfoWithInitialState :: forall t m. (Reflex t, Adjustable t m, MonadHold t m, MonadFix m) => PFState -> m (PFOutput t)
pfoWithInitialState pfState = holdPFWithInitialState pfState neverPFConfig

-- bespoke testing
tool_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Tool -> TestGuestT t m (Event t Tool))
tool_network ev = do
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig { _everythingWidgetConfig_selectTool = ev }
  return $ updated . _everythingWidget_tool $ everythingWidget

tool_test :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global) => Test
tool_test = TestLabel "tool" $ TestCase $ do
  let
    -- note, starting value is TSelect
    bs = [TPan, TSelect, TPan, TPan, TBox, TLine, TText]
    expected = [Just TPan, Just TSelect, Just TPan, Nothing, Just TBox, Just TLine, Just TText]
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
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig
    {
      _everythingWidgetConfig_potatoFlow = pfo
      , _everythingWidgetConfig_selectNew = newSelectEv
      , _everythingWidgetConfig_selectAdd = addSelectEv
    }
  return $ updated . _everythingWidget_selection $ everythingWidget

select_test :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global) => Test
select_test = TestLabel "select" $ TestCase $ do
  let
    -- TODO need to set some initial state with elements in order to test something meaningful here
    -- adding Seq.empty means no change in selection so no output event
    bs = [(False, Seq.empty)]
    expected = [Nothing]
    run = runAppSimple select_network bs
  v <- liftIO run
  (join v) @?= expected


spec :: Spec
spec = do
  describe "Everything" $ do
    fromHUnitTest $ tool_test
    fromHUnitTest $ select_test
