-- | `psx check`'s view of the compiler: the diagnostics purs reports with
-- | `--json-errors`, and their spans moved back to `.psx` sources.
module Psx.Cli.Check
  ( Diagnostic
  , Severity(..)
  , parseCompilerOutput
  , remap
  , printDiagnostic
  , diagnosticJson
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJsonArray, caseJsonNumber, caseJsonString, fromArray, fromNumber, fromObject, fromString, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Psx.Lower.PositionMap (PositionMap, toSourceRange)
import PureScript.CST.Types (SourceRange)

data Severity = Error | Warning

derive instance Eq Severity

-- | A diagnostic, with its span as cst-parser counts (lines and columns
-- | from 0).
type Diagnostic =
  { severity :: Severity
  , code :: String
  , message :: String
  , file :: Maybe String
  , range :: Maybe SourceRange
  }

-- | The diagnostics in a compiler run's output: every line that is a JSON
-- | object with `errors` and `warnings`, as `purs compile --json-errors`
-- | prints them (spago passes them through).
parseCompilerOutput :: String -> Array Diagnostic
parseCompilerOutput out = Array.concatMap fromLine (String.split (String.Pattern "\n") out)
  where
  fromLine line = fromMaybe [] do
    json <- hush (jsonParser line)
    obj <- toObject json
    errors <- Object.lookup "errors" obj
    let warnings = fromMaybe (fromArray []) (Object.lookup "warnings" obj)
    pure (list Error errors <> list Warning warnings)

  list severity = caseJsonArray [] (Array.mapMaybe (diagnostic severity))

  diagnostic severity json = do
    obj <- toObject json
    let
      str key = Object.lookup key obj >>= caseJsonString Nothing Just
    pure
      { severity
      , code: fromMaybe "" (str "errorCode")
      , message: fromMaybe "" (str "message")
      , file: str "filename"
      , range: Object.lookup "position" obj >>= position
      }

  position json = do
    obj <- toObject json
    let
      int key = Object.lookup key obj >>= caseJsonNumber Nothing (Just <<< Int.round)
    startLine <- int "startLine"
    startColumn <- int "startColumn"
    endLine <- int "endLine"
    endColumn <- int "endColumn"
    pure
      { start: { line: startLine - 1, column: startColumn - 1 }
      , end: { line: endLine - 1, column: endColumn - 1 }
      }

-- | Moves a diagnostic in a generated file to the source it came from.
remap :: { generated :: String, source :: String, positionMap :: PositionMap } -> Diagnostic -> Diagnostic
remap { generated, source, positionMap } d
  | d.file == Just generated = d { file = Just source, range = toSourceRange positionMap <$> d.range }
  | otherwise = d

-- | `file:line:column: error[Code]: message`, the message indented below
-- | when it has more than one line.
printDiagnostic :: Diagnostic -> String
printDiagnostic d = do
  let
    where' = case d.file, d.range of
      Just f, Just r -> f <> ":" <> show (r.start.line + 1) <> ":" <> show (r.start.column + 1) <> ": "
      Just f, Nothing -> f <> ": "
      Nothing, _ -> ""
    severity = case d.severity of
      Error -> "error"
      Warning -> "warning"
    code = if d.code == "" then "" else "[" <> d.code <> "]"
    lines = String.split (String.Pattern "\n") (String.trim d.message)
  case Array.uncons (Array.filter (_ /= "") (map String.trim lines)) of
    Just { head, tail } | Array.null tail -> where' <> severity <> code <> ": " <> head
    _ -> where' <> severity <> code <> ":\n" <> String.joinWith "\n" (map ("  " <> _) lines)

diagnosticJson :: Diagnostic -> Json
diagnosticJson d = fromObject $ Object.fromFoldable
  [ Tuple "severity" (fromString (if d.severity == Error then "error" else "warning"))
  , Tuple "code" (fromString d.code)
  , Tuple "message" (fromString d.message)
  , Tuple "file" (maybe (fromString "") fromString d.file)
  , Tuple "range" (maybe (fromArray []) range d.range)
  ]
  where
  range r = fromArray (map (fromNumber <<< Int.toNumber) [ r.start.line + 1, r.start.column + 1, r.end.line + 1, r.end.column + 1 ])
