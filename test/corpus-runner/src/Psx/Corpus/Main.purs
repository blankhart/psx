-- | `spago run -p psx-corpus` checks every golden under `test/corpus`;
-- | `spago run -p psx-corpus -- --accept` rewrites them. Further arguments
-- | (`area` or `area/case`) restrict the run.
module Psx.Corpus.Main (main, run) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Process as Process
import Psx.Corpus (findCorpus, parseArgs, runCorpus)
import Psx.Corpus.Stages (stages)

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  run args

run :: Array String -> Effect Unit
run args = do
  dir <- Process.cwd
  findCorpus dir >>= case _ of
    Nothing -> do
      log "psx-corpus: no test/corpus directory at or above the current directory"
      Process.setExitCode 2
    Just corpus -> do
      failures <- runCorpus stages (parseArgs args) corpus
      when (failures > 0) (Process.setExitCode 1)
