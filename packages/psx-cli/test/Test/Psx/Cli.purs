module Test.Psx.Cli where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Data.Traversable (traverse)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Psx.Cli (run)
import Psx.Cli.Check (Severity(..), parseCompilerOutput, printDiagnostic, remap)
import Psx.Cli.Explain (explain)
import Psx.Cli.GenBackend (decodeMapping, generate)
import Data.Argonaut.Parser (jsonParser)
import Psx.Lower (lower)

main :: Effect Unit
main = do
  compilerOutput
  remapping
  styles
  formatting
  explaining
  generating
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

-- `psx build` writes a styled file's stylesheet and companion, importing the
-- stylesheet from where purs puts the companion's foreign file, and removes
-- them once the file has no styles. A stylesheet psx did not write stays.
styles :: Effect Unit
styles = do
  cwd <- Process.cwd
  let
    ws = Path.concat [ cwd, "output", "psx-cli-test" ]
    src = Path.concat [ ws, "src" ]
    file name = Path.concat [ src, name ]
    styled = "module Card where\n\nimport Portico.Psx (Markup)\n\ncard :: Markup Unit\ncard =\n  <div class=\"card\">\n    <style>.card { color: red; }</style>\n  </div>\n"
    plain = "module Card where\n\nimport Portico.Psx (Markup)\n\ncard :: Markup Unit\ncard = <div class=\"card\"></div>\n"
  stale <- FS.exists ws
  when stale (FS.rm' ws { force: true, recursive: true, maxRetries: 0, retryDelay: 0 })
  FS.mkdir' src { recursive: true, mode: permsAll }
  FS.writeTextFile UTF8 (Path.concat [ ws, "spago.yaml" ]) "workspace:\n  packageSet:\n    registry: 0.0.0\n"
  FS.writeTextFile UTF8 (file "Card.psx") styled
  status <- run [ "build", src ]
  assert "the styled file builds" (status == 0)
  foreignJs <- FS.readTextFile UTF8 (file "Card/PsxStyle.js")
  assert ("the companion imports ../../src/Card.css: " <> show foreignJs)
    (String.contains (String.Pattern "import \"../../src/Card.css\";") foreignJs)
  companion <- FS.readTextFile UTF8 (file "Card/PsxStyle.purs")
  assert "the companion is Card.PsxStyle" (String.contains (String.Pattern "module Card.PsxStyle (scope) where") companion)
  sheet <- FS.readTextFile UTF8 (file "Card.css")
  assert ("the stylesheet is scoped: " <> show sheet) (String.contains (String.Pattern "@scope ([data-psx=\"Card\"])") sheet)
  FS.writeTextFile UTF8 (file "Card.psx") plain
  status' <- run [ "build", src ]
  assert "the plain file builds" (status' == 0)
  gone <- traverse (FS.exists <<< file) [ "Card.css", "Card/PsxStyle.purs", "Card/PsxStyle.js" ]
  assert "the stylesheet and companion are removed" (gone == [ false, false, false ])
  FS.writeTextFile UTF8 (file "Card.css") ".card { color: blue; }\n"
  _ <- run [ "build", src ]
  kept <- FS.readTextFile UTF8 (file "Card.css")
  assert "a hand-written stylesheet stays" (kept == ".card { color: blue; }\n")
  FS.writeTextFile UTF8 (file "Card.psx") styled
  refused <- run [ "build", src ]
  assert "a styled file does not overwrite it" (refused == 1)
  written <- FS.exists (file "Card/PsxStyle.js")
  assert "nor writes its companion" (not written)
  FS.rm' ws { force: true, recursive: true, maxRetries: 0, retryDelay: 0 }

-- `psx fmt --check` fails on a file that is not formatted and writes
-- nothing; `psx fmt` formats it in place, after which the check passes.
formatting :: Effect Unit
formatting = do
  cwd <- Process.cwd
  let
    dir = Path.concat [ cwd, "output", "psx-cli-fmt-test" ]
    file = Path.concat [ dir, "View.psx" ]
    messy = "module View where\n\nimport Portico.Psx (Markup)\n\nview :: Markup Unit\nview = <ul><li>one</li></ul>\n"
    tidy = "module View where\n\nimport Portico.Psx (Markup)\n\nview :: Markup Unit\nview =\n  <ul>\n    <li>one</li>\n  </ul>\n"
  FS.mkdir' dir { recursive: true, mode: permsAll }
  FS.writeTextFile UTF8 file messy
  unformatted <- run [ "fmt", "--check", dir ]
  assert "fmt --check fails on an unformatted file" (unformatted == 1)
  untouched <- FS.readTextFile UTF8 file
  assert "fmt --check writes nothing" (untouched == messy)
  formatted <- run [ "fmt", dir ]
  assert "fmt formats" (formatted == 0)
  text <- FS.readTextFile UTF8 file
  assert ("fmt writes the formatted file: " <> show text) (text == tidy)
  checked <- run [ "fmt", "--check", file ]
  assert "fmt --check passes on a formatted file" (checked == 0)
  FS.rm' dir { force: true, recursive: true, maxRetries: 0, retryDelay: 0 }

-- `psx explain` reads the table: an element's attributes with their
-- identifiers and types, its events, and nothing for a custom element.
explaining :: Effect Unit
explaining = do
  case explain "button" of
    Left e -> throw ("FAIL explain button: " <> e)
    Right text -> do
      let ls = map String.trim (String.split (String.Pattern "\n") text)
      assert "the heading names the backend function" (Array.head ls == Just "<button> (HTML): B.button")
      assert "an enumerated attribute lists its keywords" $ Array.any
        (\l -> String.take 5 l == "type " && String.contains (String.Pattern "B.type'") l
            && String.contains (String.Pattern "\"submit\" | \"reset\" | \"button\"") l)
        ls
      assert "global attributes are listed" (Array.any (String.contains (String.Pattern "Global attributes (")) ls)
  assert "`a` is both an HTML and an SVG element" case explain "a" of
    Right text -> String.contains (String.Pattern "<a> (SVG): B.a") text
    Left _ -> false
  assert "a custom element is not in the table" case explain "my-widget" of
    Left _ -> true
    Right _ -> false

-- `psx gen-backend` writes one function per name from the templates, the
-- most specific entry winning, and a use of each in the conformance module.
generating :: Effect Unit
generating = do
  let
    mapping = String.joinWith "\n"
      [ "{ \"module\": \"Test.Psx\", \"directory\": \"src\", \"core\": \"Test.Psx.Core\","
      , "  \"coreExports\": [\"Node\"], \"imports\": [\"Prelude\"],"
      , "  \"templates\": {"
      , "    \"element\": { \"type\": \"E\", \"code\": \"el {name}\" },"
      , "    \"voidElement\": { \"type\": \"E\", \"code\": \"void {name}\" },"
      , "    \"boolean\": { \"type\": \"{value} -> A\", \"code\": \"bool {name}\" },"
      , "    \"string\": { \"type\": \"{value} -> A\", \"code\": \"str {name}\" },"
      , "    \"number\": { \"type\": \"{value} -> A\", \"code\": \"num {name}\" },"
      , "    \"event\": { \"type\": \"H\", \"code\": \"on {name}\" } },"
      , "  \"groups\": [ { \"names\": [\"disabled\", \"id\"], \"code\": \"F.{ident}\" } ],"
      , "  \"names\": { \"id\": { \"code\": \"F.identifier\" },"
      , "    \"onInput\": { \"type\": \"I\", \"code\": \"F.input\", \"sample\": \"\\\\_ -> msg\" } },"
      , "  \"conformance\": { \"nodes\": \"Ns\", \"handlers\": \"Hs\" } }"
      ]
  case jsonParser mapping >>= decodeMapping >>= generate "test.json" of
    Left e -> throw ("FAIL gen-backend: " <> e)
    Right generated -> do
      let
        has text line = Array.elem line (String.split (String.Pattern "\n") text)
        backend = has generated.backend
        conformance = has generated.conformance
      assert "the header names the mapping" (backend "-- Generated by psx gen-backend from test.json and psx-html-data. Do not edit.")
      assert "the core is re-exported" (backend "import Test.Psx.Core (Node) as Core")
      assert "an element" (backend "button = el \"button\"")
      assert "a void element" (backend "br = void \"br\"")
      assert "a group's name" (backend "disabled = F.disabled")
      assert "a name's own entry wins over its group" (backend "id = F.identifier")
      assert "a number attribute" (backend "tabindex :: Int -> A")
      assert "an enumeration is a string" (backend "popover :: String -> A")
      assert "an event" (backend "onClick = on \"click\"")
      assert "an SVG element" (backend "textEl = el \"text\"")
      assert "a boolean use" (conformance "  [ B.button [ B.disabled true ] []")
      assert "an attribute shared by HTML and SVG is used in each" (conformance "  , B.g [ B.id \"\" ] []")
      assert "an enumeration's use is its first keyword" (conformance "  [ B.div [ B.popover \"auto\" ] []")
      assert "an event's sample" (conformance "  [ B.div [ B.onInput (\\_ -> msg) ] []")
  case jsonParser "{ \"module\": \"M\" }" >>= decodeMapping of
    Left _ -> pure unit
    Right _ -> throw "FAIL a mapping without templates is rejected"
