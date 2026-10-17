-- | The format stage: `expected.fmt` is what `psx fmt` makes of the input,
-- | or the errors that stop it.
module Psx.Corpus.Format (format) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Psx.Corpus (Stage)
import Psx.Format (formatPsx, formatPsxt)

format :: Stage
format =
  { name: "format"
  , golden: "expected.fmt"
  , applies: const true
  , run: \c -> pure case (if c.inputFile == "input.psxt" then formatPsxt else formatPsx) c.input of
      Right formatted -> formatted
      Left errors -> "-- psx fmt failed\n" <> foldMap error errors
  }
  where
  error e =
    "-- " <> show (e.range.start.line + 1) <> ":" <> show (e.range.start.column + 1)
      <> " "
      <> e.message
      <> "\n"
