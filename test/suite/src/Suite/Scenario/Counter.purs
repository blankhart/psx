-- | `App.Counter` from the design doc: holes, `if` in a hole, a keyed list
-- | and a scoped style. The DOM contracts leave out the style's `data-psx`
-- | stamp (see `Suite.Dom`).
module Suite.Scenario.Counter (counter) where

import Suite.Scenario (Scenario, Step(..))

counter :: Scenario
counter =
  { name: "counter"
  , fixture: """{ "label": "Clicks", "max": 3 }"""
  , steps:
      [ Expect "initial"
      , Click "increment"
      , Expect "one"
      , Click "increment"
      , Click "increment"
      , Expect "max"
      , Click "decrement"
      , Expect "two"
      , Click "reset"
      , Expect "initial"
      , Measure
          { name: "count up and reset"
          , runs: 5
          , setup: []
          , steps: [ Click "increment", Click "increment", Click "increment", Click "reset" ]
          }
      , Expect "initial"
      ]
  }
