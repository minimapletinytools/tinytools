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



-- bespoke testing

tool_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t Tool -> TestGuestT t m (Event t Tool))
tool_network ev = do
  everythingWidget <- holdEverythingWidget $ emptyEverythingWidgetConfig { _everythingWidgetConfig_selectTool = ev }
  return $ updated . _everythingWidget_tool $ everythingWidget

-- TODO maybe drop the `t ~ SpiderTimeline Global` constraint
-- you'll need to modify reflex-test-host for this
tool_test :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global) => Test
tool_test = TestLabel "tool" $ TestCase $ do
  let
    -- note, starting value is TSelect
    bs = [TPan, TSelect, TPan, TPan, TBox, TLine, TText]
    expected = [Just TPan, Just TSelect, Just TPan, Nothing, Just TBox, Just TLine, Just TText]
    run = runAppSimple tool_network bs
  v <- liftIO run
  (join v) @?= expected


spec :: Spec
spec = do
  describe "Everything" $ do
    fromHUnitTest $ tool_test
