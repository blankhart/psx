-- | What a scenario is. A scenario names no framework: it is a fixture the
-- | app starts from and the steps a user takes, addressed by `data-testid`,
-- | so every implementation of it runs the same steps.
module Suite.Scenario
  ( Scenario
  , Step(..)
  , TestId
  ) where

-- | The `data-testid` of the element a step acts on.
type TestId = String

data Step
  -- | Click the element.
  = Click TestId
  -- | Replace the text in the input, as a user typing it would.
  | Type TestId String
  -- | The app's normalised DOM must equal the scenario's committed contract
  -- | of this name, `test/suite/scenarios/<scenario>/<name>.html`.
  | Expect String
  -- | Run the steps and time them, in milliseconds, under this name.
  | Measure String (Array Step)

-- | `fixture` is JSON, handed to the app's `main` as its argument.
type Scenario =
  { name :: String
  , fixture :: String
  , steps :: Array Step
  }
