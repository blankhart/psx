-- | A todo list: text input, keyed add, remove and toggle, filtering, and
-- | boolean attributes.
module Suite.Scenario.Todo (todo) where

import Suite.Scenario (Scenario, Step(..))

todo :: Scenario
todo =
  { name: "todo"
  , fixture:
      """{ "todos":
           [ { "id": 1, "text": "Parse markup", "done": true }
           , { "id": 2, "text": "Lower to PureScript", "done": false }
           ]
         }"""
  , steps:
      [ Expect "initial"
      , Type "draft" "Run the suite"
      , Expect "typed"
      , Click "add"
      , Expect "added"
      , Click "toggle-2"
      , Expect "toggled"
      , Click "active"
      , Expect "active"
      , Click "completed"
      , Expect "completed"
      , Click "all"
      , Click "remove-1"
      , Expect "removed"
      , Measure
          { name: "filter"
          , runs: 5
          , setup: []
          , steps: [ Click "active", Click "completed", Click "all" ]
          }
      , Expect "removed"
      ]
  }
