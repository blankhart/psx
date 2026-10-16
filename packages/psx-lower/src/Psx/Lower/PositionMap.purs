-- | Where each piece of a lowered `.purs` file came from.
-- |
-- | A `Copy` segment is source copied verbatim: plain PureScript and the
-- | expressions in holes. A `Generated` segment is code the lowering wrote
-- | for a piece of markup (an element's function, an attribute, a text
-- | node), and points at that markup. Positions are cst-parser's: lines from
-- | 0, columns in code points from 0. The JSON form counts both from 1, as
-- | purs reports them.
module Psx.Lower.PositionMap
  ( PositionMap
  , Segment
  , SegmentKind(..)
  , toSource
  , toSourceRange
  , printJson
  , before
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import PureScript.CST.Types (SourcePos, SourceRange)

data SegmentKind = Copy | Generated

derive instance Eq SegmentKind

type Segment =
  { kind :: SegmentKind
  , generated :: SourceRange
  , source :: SourceRange
  }

-- | `source` and `generated` name the two files; `segments` are in the
-- | order of the generated file.
type PositionMap =
  { source :: String
  , generated :: String
  , segments :: Array Segment
  }

-- | The source position of a generated one. Inside a copy it is exact; inside
-- | generated code it is the start of the markup that produced it; anywhere
-- | else (the header, the backend import, punctuation between segments) it
-- | is the end of the nearest segment before it.
toSource :: PositionMap -> SourcePos -> SourcePos
toSource pm pos =
  case Array.find (\s -> s.kind == Copy && contains s.generated) pm.segments of
    Just s -> fromCopy s
    Nothing -> case smallest (Array.filter (\s -> s.kind == Generated && contains s.generated) pm.segments) of
      Just s -> s.source.start
      Nothing -> case Array.last (Array.filter (\s -> before s.generated.end pos || s.generated.end == pos) pm.segments) of
        Just s -> s.source.end
        Nothing -> { line: 0, column: 0 }
  where
  contains r = not (before pos r.start) && before pos r.end

  fromCopy s = do
    let dl = pos.line - s.generated.start.line
    { line: s.source.start.line + dl
    , column: if dl == 0 then s.source.start.column + (pos.column - s.generated.start.column) else pos.column
    }

  smallest = Array.head <<< Array.sortWith \s ->
    [ s.generated.end.line - s.generated.start.line, s.generated.end.column - s.generated.start.column ]

-- | A generated range in the source. A range whose ends fall in different
-- | segments is cut down to its start's segment, so it never runs backwards.
toSourceRange :: PositionMap -> SourceRange -> SourceRange
toSourceRange pm r = do
  let
    start = toSource pm r.start
    end = toSource pm r.end
  { start, end: if before end start then start else end }

-- | Whether a position comes before another. (Records order their fields
-- | alphabetically, which would put the column first.)
before :: SourcePos -> SourcePos -> Boolean
before a b = a.line < b.line || (a.line == b.line && a.column < b.column)

printJson :: PositionMap -> String
printJson pm = intercalate "\n"
  ( [ "{"
    , "  \"version\": 1,"
    , "  \"source\": " <> show pm.source <> ","
    , "  \"generated\": " <> show pm.generated <> ","
    , "  \"segments\": ["
    ]
      <> Array.mapWithIndex segment pm.segments
      <> [ "  ]", "}" ]
  ) <> "\n"
  where
  lastIndex = Array.length pm.segments - 1

  segment i s =
    "    { \"kind\": " <> kind s.kind
      <> ", \"generated\": "
      <> range s.generated
      <> ", \"source\": "
      <> range s.source
      <> " }"
      <> (if i < lastIndex then "," else "")

  kind Copy = "\"copy\""
  kind Generated = "\"generated\""

  range r = "[" <> intercalate ", " (map show [ r.start.line + 1, r.start.column + 1, r.end.line + 1, r.end.column + 1 ]) <> "]"
