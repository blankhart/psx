-- | The stages the corpus runs. Each build step adds its own stage here
-- | (the CST printer, the lowering, the position map, the formatter).
module Psx.Corpus.Stages (stages) where

import Psx.Corpus (Stage)
import Psx.Corpus.Format (format)
import Psx.Corpus.Lower (css, lower, positionMap, same, warnings)
import Psx.Corpus.Parse (parse, parsePsxt)

stages :: Array Stage
stages =
  [ parse
  , parsePsxt
  , lower
  , warnings
  , positionMap
  , css
  , same
  , format
  ]
