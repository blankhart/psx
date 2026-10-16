module Test.Psx.Cli where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Psx.Cli.Check (Severity(..), parseCompilerOutput, printDiagnostic, remap)
import Psx.Lower (lower)

main :: Effect Unit
main = do
  compilerOutput
  remapping
  log "psx-cli: all tests passed"

assert :: String -> Boolean -> Effect Unit
assert what ok = unless ok (throw ("FAIL " <> what))

-- What `purs compile --json-errors` prints, among spago's own lines.
output :: String
output =
  "Building...\n"
    <> "{\"warnings\":[{\"position\":{\"startLine\":3,\"startColumn\":1,\"endLine\":3,\"endColumn\":15},\"message\":\"The import of Data.Maybe is redundant\\n\",\"errorCode\":\"UnusedImport\",\"errorLink\":\"\",\"filename\":\"src/Other.purs\",\"moduleName\":\"Other\",\"suggestion\":null,\"allSpans\":[]}],"
    <> "\"errors\":[{\"position\":{\"startLine\":9,\"startColumn\":30,\"endLine\":9,\"endColumn\":35},\"message\":\"Could not match type\\n\\n    Int\\n\\nwith type\\n\\n    String\\n\",\"errorCode\":\"TypesDoNotUnify\",\"errorLink\":\"\",\"filename\":\"src/View.purs\",\"moduleName\":\"View\",\"suggestion\":null,\"allSpans\":[]}]}\n"
    <> "✘ Failed to build.\n"

compilerOutput :: Effect Unit
compilerOutput = do
  let diagnostics = parseCompilerOutput output
  assert "an error and a warning are read" (Array.length diagnostics == 2)
  case Array.find (\d -> d.severity == Error) diagnostics of
    Nothing -> throw "FAIL the error is read"
    Just d -> do
      assert "the error's code" (d.code == "TypesDoNotUnify")
      assert "the error's file" (d.file == Just "src/View.purs")
      assert "the error's span, counted from 0" (d.range == Just { start: { line: 8, column: 29 }, end: { line: 8, column: 34 } })
      assert ("the printed error: " <> printDiagnostic d)
        (Array.head (lines (printDiagnostic d)) == Just "src/View.purs:9:30: error[TypesDoNotUnify]:")
  where
  lines = String.split (String.Pattern "\n")

-- An error in a generated file moves to the hole it came from.
remapping :: Effect Unit
remapping = do
  let
    input = "module View where\n\nimport Prelude\nimport Portico.Psx (Markup)\n\nview :: Int -> Markup Unit\nview n =\n  <p>\n    {\"n = \" <> n}\n  </p>\n"
  case lower { sourceName: "View.psx", generatedName: "View.purs" } input of
    Left _ -> throw "FAIL the example lowers"
    Right result -> do
      let
        generated = result.purs
        d =
          { severity: Error
          , code: "TypesDoNotUnify"
          , message: ""
          , file: Just "src/View.purs"
          , range: Just { start: { line: 8, column: 15 }, end: { line: 8, column: 16 } }
          }
        moved = remap { generated: "src/View.purs", source: "src/View.psx", positionMap: result.positionMap } d
      assert ("the hole is on its own line in " <> show generated)
        (Array.index (String.split (String.Pattern "\n") generated) 8 # map (String.contains (String.Pattern "\"n = \" <> n")) # eq (Just true))
      assert "the error is in the source" (moved.file == Just "src/View.psx")
      assert ("the error is at the hole's `n`, not " <> show moved.range)
        (map _.start moved.range == Just { line: 8, column: 15 })
