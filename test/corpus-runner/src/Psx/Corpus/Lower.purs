-- | The lowering stages: `expected.purs` is the lowered module, or the
-- | errors that stopped it, and `expected.map.json` its position map.
-- | Corpus cases under `parse/` are left to the parse stage.
module Psx.Corpus.Lower (lower, positionMap) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Psx.Corpus (Case, Stage)
import Psx.Lower as Lower
import Psx.Lower.PositionMap as PositionMap

applies :: Case -> Boolean
applies c = c.inputFile == "input.psx" && c.area /= "parse"

lowered :: Case -> _
lowered c = Lower.lower { sourceName: c.inputFile, generatedName: "input.purs" } c.input

lower :: Stage
lower =
  { name: "lower"
  , golden: "expected.purs"
  , applies
  , run: \c -> pure case lowered c of
      Right result -> result.purs
      Left errors -> "-- psx lower failed\n" <> foldMap error errors
  }
  where
  error e =
    "-- " <> show (e.range.start.line + 1) <> ":" <> show (e.range.start.column + 1)
      <> " "
      <> e.message
      <> "\n"

positionMap :: Stage
positionMap =
  { name: "position map"
  , golden: "expected.map.json"
  , applies: \c -> applies c && case lowered c of
      Right _ -> true
      Left _ -> false
  , run: \c -> pure case lowered c of
      Right result -> PositionMap.printJson result.positionMap
      Left _ -> ""
  }
