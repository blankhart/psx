module Test.Psx.Lower where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as SCU

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Psx.Cst.Source (offsetOf, source)
import Psx.Lower (lower)
import Psx.Lower.Entities as Entities
import Psx.Lower.PositionMap (SegmentKind(..), toSource)
import Psx.Lower.Whitespace (normalise)
import Psx.Cst (parseModule)
import PureScript.CST as CST
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Module(..), ModuleBody(..))

main :: Effect Unit
main = do
  whitespace
  entities
  positionMap
  files <- sources
  for_ files lowered
  log $ "psx-lower: all tests passed (" <> show (Array.length files) <> " files lowered)"

assert :: String -> Boolean -> Effect Unit
assert what ok = unless ok (throw ("FAIL " <> what))

whitespace :: Effect Unit
whitespace = do
  let
    cases =
      [ { text: "Hello", expect: "Hello" }
      , { text: " a  b ", expect: " a  b " }
      , { text: "\n    ", expect: "" }
      , { text: "\n  one\n  two\n", expect: "one two" }
      , { text: "a \n\n   b", expect: "a b" }
      , { text: ": ", expect: ": " }
      , { text: "x\r\n  y", expect: "x y" }
      , { text: "\n  \xa0\n", expect: "\xa0" }
      ]
  for_ cases \c ->
    assert ("whitespace policy on " <> show c.text <> " gives " <> show (normalise c.text)) (normalise c.text == c.expect)

entities :: Effect Unit
entities = do
  let
    cases =
      [ { text: "a &amp; b", expect: "a & b" }
      , { text: "&lt;&gt;&quot;&apos;", expect: "<>\"'" }
      , { text: "&#65;&#x42;&#X43;", expect: "ABC" }
      , { text: "&#x1F600;", expect: "😀" }
      , { text: "&nope; & &;", expect: "&nope; & &;" }
      , { text: "&#xD800;", expect: "&#xD800;" }
      ]
  for_ cases \c ->
    assert ("references in " <> show c.text <> " give " <> show (Entities.decode c.text)) (Entities.decode c.text == c.expect)

-- A position in a hole's expression maps back to where it was written.
positionMap :: Effect Unit
positionMap = do
  let
    input = "module M where\n\nimport Portico.Psx (Markup)\n\nview n =\n  <p class=\"x\">\n    {show (n + 1)}\n  </p>\n"
  case lower { sourceName: "M.psx", generatedName: "M.purs" } input of
    Left _ -> throw "FAIL the position map example lowers"
    Right result -> do
      let
        out = source result.purs
        at = SCU.indexOf (String.Pattern "n + 1") result.purs
      case at of
        Nothing -> throw "FAIL the hole's expression is in the output"
        Just offset -> do
          let pos = toSource result.positionMap (outPos out offset)
          assert ("the hole's expression maps to 7:12, not " <> show pos) (pos == { line: 6, column: 11 })
  where
  outPos out offset = do
    let src = out
    let lineStarts = Array.filter (_ <= offset) src.lineStarts
    { line: Array.length lineStarts - 1
    , column: String.length (SCU.slice (Array.foldl max 0 lineStarts) offset src.text)
    }

-- Every `.psx` file the corpus lowers and every example.
sources :: Effect (Array FilePath)
sources = do
  root <- workspaceRoot
  let
    lowerDir = Path.concat [ root, "test", "corpus", "lower" ]
    examples = Path.concat [ root, "examples", "portico", "src" ]
  cases <- FS.readdir lowerDir
  corpus <- Array.filterA FS.exists (cases <#> \c -> Path.concat [ lowerDir, c, "input.psx" ])
  exampleFiles <- map (\f -> Path.concat [ examples, f ]) <<< Array.filter (\f -> Path.extname f == ".psx") <$> FS.readdir examples
  pure (Array.sort corpus <> Array.sort exampleFiles)

workspaceRoot :: Effect FilePath
workspaceRoot = Process.cwd >>= go
  where
  go dir = do
    found <- FS.exists (Path.concat [ dir, "test", "corpus" ])
    if found then pure dir
    else if Path.dirname dir == dir then throw "no workspace root above the current directory"
    else go (Path.dirname dir)

-- What a lowering must be: plain PureScript that cst-parser parses, whose
-- copied segments are the source's text, and whose lines after the imports
-- are the source's lines.
lowered :: FilePath -> Effect Unit
lowered file = do
  input <- FS.readTextFile UTF8 file
  case lower { sourceName: "input.psx", generatedName: "input.purs" } input of
    Left _ -> pure unit -- an error case
    Right result -> do
      case CST.parseModule result.purs of
        CST.ParseSucceeded _ -> pure unit
        _ -> throw ("FAIL the lowering of " <> file <> " is not PureScript cst-parser parses")
      let
        src = source input
        out = source result.purs
        copies = Array.filter (\s -> s.kind == Copy) result.positionMap.segments
        text s r = SCU.slice (offsetOf s r.start) (offsetOf s r.end) s.text
        firstDecl = case parseModule input of
          { module: Just (Module { body: ModuleBody body }) } -> (\d -> (rangeOf d).start.line) <$> Array.head body.decls
          _ -> Nothing
      for_ copies \seg ->
        assert ("a copy in " <> file <> " is the source's text") (text out seg.generated == text src seg.source)
      for_ firstDecl \decl ->
        for_ (Array.filter (\s -> s.source.start.line >= decl) copies) \seg ->
          assert
            ("line " <> show (seg.source.start.line + 1) <> " of " <> file <> " stays line " <> show (seg.source.start.line + 1) <> " (is " <> show (seg.generated.start.line + 1) <> ")")
            (seg.generated.start.line == seg.source.start.line)

