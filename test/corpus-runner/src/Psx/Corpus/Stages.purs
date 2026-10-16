-- | The stages the corpus runs. Each build step adds its own stage here
-- | (the CST printer, the lowering, the position map, the formatter).
module Psx.Corpus.Stages (stages) where

import Prelude

import Psx.Corpus (Stage)

stages :: Array Stage
stages =
  [ echo
  ]

-- | Placeholder until the first real stage lands: copies the input of the
-- | cases in the `dummy` area, so the runner and `--accept` have something
-- | to check.
echo :: Stage
echo =
  { name: "echo"
  , golden: "expected.echo"
  , applies: \c -> c.area == "dummy"
  , run: \c -> pure c.input
  }
