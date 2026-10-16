-- Unit tests of `update` functions, which run without a browser because the
-- effects they return are data. Run with `npm run test:unit`.
module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Examples.Dice as Dice
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual
    { actual: Dice.update Nothing Dice.Roll
    , expected: Tuple Nothing [ Dice.RollDie ]
    }
  assertEqual
    { actual: Dice.update (Just 2) (Dice.Update 5)
    , expected: Tuple (Just 5) []
    }
  log "Dice.update: ok"
