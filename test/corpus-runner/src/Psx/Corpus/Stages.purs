-- | The stages the corpus runs. Each build step adds its own stage here
-- | (the CST printer, the lowering, the position map, the formatter).
module Psx.Corpus.Stages (stages) where



import Psx.Corpus (Stage)
import Psx.Corpus.Parse (parse)

stages :: Array Stage
stages =
  [ parse
  ]
