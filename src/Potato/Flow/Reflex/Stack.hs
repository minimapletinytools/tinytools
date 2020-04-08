module Potato.Flow.Reflex.Stack (

) where

import           Relude

import           Reflex


-- Note, these sigs prob won't work due to not being able to access previous value of a dyn after it changes

-- | element added to stack
pushEv :: Dynamic t [a] -> Event t a
pushEv = undefined

popEv :: Dynamic t [a] -> Event t a
popEv = undefined
