-- | SVG elements and attributes inside HTML, written with no prefixes: a
-- | bar chart with a line through the bars' tops, where clicking a bar
-- | selects it and "Grow" adds one to the selected value. The contracts
-- | name elements in the SVG namespace `svg:<name>` (see `Suite.Driver`),
-- | so an element created in the HTML namespace fails them.
module Suite.Scenario.Svg (svg) where

import Suite.Scenario (Scenario, Step(..))

svg :: Scenario
svg =
  { name: "svg"
  , fixture: """{ "values": [ 3, 7, 4, 9, 5 ], "max": 12 }"""
  , steps:
      [ Expect "initial"
      , Click "bar-2"
      , Expect "selected"
      , Click "grow"
      , Click "grow"
      , Expect "grown"
      , Click "bar-0"
      , Click "grow"
      , Expect "reselected"
      , Measure
          { name: "select bars"
          , runs: 5
          , setup: []
          , steps: [ Click "bar-1", Click "bar-3", Click "bar-0" ]
          }
      , Expect "reselected"
      ]
  }
