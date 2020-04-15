
module Potato.Flow.Reflex.Manipulators (
  ManipulatorCmd(..)
) where

import           Relude


data ManipulatorCmd t = ManipulatorCmd

data BoxManipulator = BoxManipulator {
  -- behavior to read state of manipulator
  -- events that stage an action on the stack
}
