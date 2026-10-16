-- | `spago test -p psx-corpus` checks every golden, without rewriting.
module Test.Psx.Corpus (main) where

import Prelude

import Effect (Effect)
import Psx.Corpus.Main (run)

main :: Effect Unit
main = run []
