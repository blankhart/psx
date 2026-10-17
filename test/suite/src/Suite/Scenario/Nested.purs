-- | Child components with their own state: Halogen slots, Screamer
-- | components and Portico composition. Each counter keeps its own count,
-- | which survives its siblings being added and removed; the parent adds
-- | and removes counters and shows the total, which the children report.
module Suite.Scenario.Nested (nested) where

import Suite.Scenario (Scenario, Step(..))

nested :: Scenario
nested =
  { name: "nested"
  , fixture:
      """{ "counters":
           [ { "id": 1, "label": "Apples", "start": 1 }
           , { "id": 2, "label": "Pears", "start": 5 }
           ]
         }"""
  , steps:
      [ Expect "initial"
      , Click "increment-1"
      , Click "increment-1"
      , Expect "incremented"
      , Click "add"
      , Click "increment-3"
      , Click "decrement-2"
      , Expect "added"
      , Click "remove-1"
      , Expect "removed"
      , Click "add"
      , Expect "readded"
      , Measure
          { name: "count up and down"
          , runs: 5
          , setup: []
          , steps: [ Click "increment-2", Click "increment-3", Click "decrement-2", Click "decrement-3" ]
          }
      , Expect "readded"
      ]
  }
