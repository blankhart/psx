-- | Medians and spread, for timings.
module Suite.Stats
  ( Summary
  , summarise
  , quantile
  ) where

import Prelude

import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)

-- | `q1` and `q3` are the quartiles, so `q3 - q1` is the spread.
type Summary = { n :: Int, median :: Number, q1 :: Number, q3 :: Number, min :: Number, max :: Number }

summarise :: Array Number -> Summary
summarise samples =
  { n: Array.length sorted
  , median: quantile 0.5 sorted
  , q1: quantile 0.25 sorted
  , q3: quantile 0.75 sorted
  , min: fromMaybe 0.0 (Array.head sorted)
  , max: fromMaybe 0.0 (Array.last sorted)
  }
  where
  sorted = Array.sort samples

-- | The quantile of sorted samples, interpolating between the two nearest.
quantile :: Number -> Array Number -> Number
quantile p sorted = case Array.length sorted of
  0 -> 0.0
  n ->
    let
      at = p * toNumber (n - 1)
      i = floor at
      lo = fromMaybe 0.0 (Array.index sorted i)
      hi = fromMaybe lo (Array.index sorted (i + 1))
    in
      lo + (hi - lo) * (at - toNumber i)
