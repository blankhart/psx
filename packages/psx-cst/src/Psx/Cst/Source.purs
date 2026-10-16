-- | Source text with positions as cst-parser counts them: zero-based lines,
-- | and columns in code points. Offsets are in UTF-16 code units, as
-- | `Data.String.CodeUnits` counts them.
module Psx.Cst.Source
  ( Source
  , source
  , posOf
  , offsetOf
  , slice
  , charAt
  , startsWith
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import PureScript.CST.Types (SourcePos)

type Source =
  { text :: String
  -- | The offset at which each line starts.
  , lineStarts :: Array Int
  }

source :: String -> Source
source text =
  { text
  , lineStarts: Array.scanl (\start line -> start + SCU.length line + 1) 0 lines # Array.cons 0 # Array.dropEnd 1
  }
  where
  lines = String.split (String.Pattern "\n") text

-- | The position of an offset.
posOf :: Source -> Int -> SourcePos
posOf src offset = do
  let
    line = lineOf 0 (Array.length src.lineStarts - 1)
    start = fromMaybe 0 (Array.index src.lineStarts line)
  { line, column: String.length (SCU.slice start offset src.text) }
  where
  -- The last line starting at or before the offset (binary search).
  lineOf lo hi
    | lo >= hi = lo
    | otherwise = do
        let mid = (lo + hi + 1) / 2
        if fromMaybe 0 (Array.index src.lineStarts mid) <= offset then lineOf mid hi else lineOf lo (mid - 1)

-- | The offset of a position.
offsetOf :: Source -> SourcePos -> Int
offsetOf src pos = do
  let start = fromMaybe (SCU.length src.text) (Array.index src.lineStarts pos.line)
  start + SCU.length (String.take pos.column (SCU.drop start src.text))

slice :: Source -> Int -> Int -> String
slice src from to = SCU.slice from to src.text

charAt :: Source -> Int -> Maybe Char
charAt src i = SCU.charAt i src.text

startsWith :: Source -> Int -> String -> Boolean
startsWith src i s = SCU.slice i (i + SCU.length s) src.text == s
