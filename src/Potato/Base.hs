module Potato.Base(

) where

-- TODO switch to math library maybe?
newtype XY = XY { unXY :: (Int, Int) }
newtype LSize = LSize { unLSize :: XY }
newtype LPoint = LPoint { unLPoint :: XY }
newtype VPoint = VPoint (Int, Int)
data PFTrans = PFTrans {
  logical   :: LPoint
  , virtual :: VPoint
}

type Raycast = VPoint -> Bool

data Renderer = Renderer {
  -- TODO
}

data Box = Box {
  ul     :: LPoint
  , size :: LSize
}

data PFParam = PFParam {

}

-- TODO
--class Manipulator e where

data BoxManipulator {
}

-- Elt
data PFElt = PFElt {
  -- never modify these directly
  children  :: [PFElt]
  , trans   :: PFTrans
  , raycast :: Raycast
  , boxes   :: [(Box, Renderer)]
  , params  :: [PFParam]

  -- always modify through these
  , boxManipulator :: BoxManipulator
  --, manipulator ::
}


-- Manipulator

-- Action
-- | Actions
-- PFActions hold direct references to elements they act
data PFAction {
  -- fire off events to do/undo an action
  doAction :: IO ()
  , undoAction :: IO ()
  , showAction :: String -- i.e. to print out history
}

-- probably want something like ActionStack as input/output to all these methods
stageAction :: PFAction -> IO ()
stageAction = undefined

pushAction :: PFAction -> IO ()
pushAction = undefined

undoAction :: IO ()
undoAction = undefined

main :: IO ()
main = do
  -- initialize front end (or mock front end) -> MonadHold
  --
  return ()
