-- | The js-framework-benchmark operations on 1,000 and 10,000 rows: create,
-- | replace, update every 10th row, select, swap, remove, append and clear.
-- | The markup is the benchmark's, with a `data-testid` on each button and
-- | on each row's two links. Labels come from `Suite.Shared.Table`, the
-- | same pseudo-random sequence in every app, so the contracts are exact.
module Suite.Scenario.Table (table) where

import Prelude

import Suite.Scenario (Measure, Scenario, Step(..))

table :: Scenario
table =
  { name: "table"
  , fixture: """{ "seed": 42 }"""
  , steps:
      [ Expect "empty"
      , Click "run"
      , Expect "created"
      , Click "update"
      , Expect "updated"
      , ClickNth "select-" 4
      , Expect "selected"
      , Click "swaprows"
      , Expect "swapped"
      , ClickNth "remove-" 2
      , Expect "removed"
      , Click "add"
      , Expect "appended"
      , Click "clear"
      , Expect "empty"
      , Click "runlots"
      , Expect "lots"
      , Click "clear"
      , Expect "empty"
      ]
      <> map Measure (measures "1,000" "run" <> measures "10,000" "runlots")
      <> [ Click "clear", Expect "empty" ]
  }

-- | Each operation on a table of this size, which the button creates.
measures :: String -> String -> Array Measure
measures size create =
  [ measure ("create " <> size <> " rows") [ Click "clear" ] [ Click create ]
  , measure ("replace " <> size <> " rows") [ Click create ] [ Click create ]
  , measure ("update every 10th of " <> size <> " rows") [ Click create ] [ Click "update" ]
  , measure ("select a row of " <> size) [ Click create ] [ ClickNth "select-" 1 ]
  , measure ("swap two of " <> size <> " rows") [ Click create ] [ Click "swaprows" ]
  , measure ("remove a row of " <> size) [ Click create ] [ ClickNth "remove-" 1 ]
  , measure ("append 1,000 rows to " <> size) [ Click create ] [ Click "add" ]
  , measure ("clear " <> size <> " rows") [ Click create ] [ Click "clear" ]
  ]
  where
  measure name setup steps = { name, runs: 3, setup, steps }
