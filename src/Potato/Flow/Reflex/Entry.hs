
module Potato.Flow.Reflex.Entry (

) where

import           Relude

import           Reflex

import           Data.Aeson
import qualified Data.ByteString.Lazy     as LBS

import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts


type LoadFileEvent t =  Event t LBS.ByteString
type SetWSEvent t = Event t SEltTree

loadWSFromFile :: (Reflex t) => LoadFileEvent t -> SetWSEvent t
loadWSFromFile = fmapMaybe decode

type NewREltEvent t = Event t (REltTree t)

-- need a data structure that allows us to do this
--Dynamic t [REltTree t] -> NewREltEvent
