-- | `App.Counter` from the design doc: holes, `if` in a hole and a keyed
-- | list. Its scoped style joins it once `.psxt` and styles land.
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
      , Measure "count up and reset"
          [ Click "increment", Click "increment", Click "increment", Click "reset" ]
      , Expect "initial"
      ]
  }
