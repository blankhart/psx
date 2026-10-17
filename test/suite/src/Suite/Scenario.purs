-- | What a scenario is. A scenario names no framework: it is a fixture the
-- | app starts from and the steps a user takes, addressed by `data-testid`,
-- | so every implementation of it runs the same steps.
module Suite.Scenario
  ( Scenario
  , Step(..)
  , Measure
  , TestId
  ) where

-- | The `data-testid` of the element a step acts on.
type TestId = String

data Step
  -- | Click the element.
  = Click TestId
  -- | Click the element at this index (from 0) among those whose
  -- | `data-testid` starts with this prefix, such as a table's second row.
  | ClickNth TestId Int
  -- | Replace the text in the input, as a user typing it would.
  | Type TestId String
  -- | Press a key in the element, by its `KeyboardEvent.key` name.
  | Press TestId String
  -- | The app's normalised DOM must equal the scenario's committed contract
  -- | of this name, `test/suite/scenarios/<scenario>/<name>.html`.
  | Expect String
  -- | `Expect`, for a step whose effect arrives later: the DOM must come to
  -- | equal the contract within a few seconds.
  | Await String
  -- | Time some steps (see `Measure`).
  | Measure Measure

-- | `runs` times: the `setup` steps, untimed, then the `steps`, each timed
-- | inside the page from the event to the next frame after it. The report
-- | gives the median of the runs.
type Measure =
  { name :: String
  , runs :: Int
  , setup :: Array Step
  , steps :: Array Step
  }

-- | `fixture` is JSON, handed to the app's `main` as its argument.
type Scenario =
  { name :: String
  , fixture :: String
  , steps :: Array Step
  }
