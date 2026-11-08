module Test.Psx.Lower where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
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
import Psx.Cst.Psxt (parsePsxt)
import Psx.Cst.Source (offsetOf, posOf, source)
import Psx.Lower (lower, lowerPsxt)
import Psx.Lower.Css as Css
import Psx.Lower.Entities as Entities
import Psx.Lower.PositionMap (SegmentKind(..), toSource)
import Psx.Lower.Whitespace (normalise)
import Psx.Cst (parseModule)
import PureScript.CST as CST
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Module(..), ModuleBody(..))

main :: Effect Unit
main = do
  attributeWarnings
  whitespace
  entities
  positionMap
  css
  psxt
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

-- Every `.psx` and `.psxt` file the corpus lowers and every example.
sources :: Effect (Array FilePath)
sources = do
  root <- workspaceRoot
  let
    areas = [ "lower", "psxt" ]
    examples = Path.concat [ root, "examples", "react", "src" ]
  corpus <- Array.concat <$> for areas \area -> do
    let dir = Path.concat [ root, "test", "corpus", area ]
    cases <- FS.readdir dir
    Array.filterA FS.exists (Array.concatMap (\c -> [ Path.concat [ dir, c, "input.psx" ], Path.concat [ dir, c, "input.psxt" ] ]) cases)
  exampleFiles <- map (\f -> Path.concat [ examples, f ]) <<< Array.filter (\f -> Path.extname f == ".psx" || Path.extname f == ".psxt") <$> FS.readdir examples
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
-- are the source's lines, shifted by at most one when an added import finds
-- no blank line to take. In a `.psxt` file that holds for the script.
lowered :: FilePath -> Effect Unit
lowered file = do
  input <- FS.readTextFile UTF8 file
  let
    isPsxt = Path.extname file == ".psxt"
    names = { sourceName: Path.basename file, generatedName: "input.purs" }
  case (if isPsxt then lowerPsxt names input else lower names input) of
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
        parsed =
          if isPsxt then (parsePsxt input).file <#> \f ->
            { m: f.script.module
            , end: (posOf src f.script.contentEnd).line
            -- The template's signatures move to the templates.
            , moved: map _.range.start.line (Array.mapMaybe _.declaration f.templates)
            }
          else (parseModule input).module <#> \m -> { m, end: Array.length src.lineStarts, moved: [] }
        firstDecl = parsed >>= \{ m: Module { body: ModuleBody body } } -> (\d -> (rangeOf d).start.line) <$> Array.head body.decls
        scriptEnd = maybe 0 _.end parsed
        moved = maybe [] _.moved parsed
      for_ copies \seg ->
        assert ("a copy in " <> file <> " is the source's text") (text out seg.generated == text src seg.source)
      for_ firstDecl \decl -> do
        let
          kept = Array.filter (\s -> s.source.start.line >= decl && s.source.start.line < scriptEnd && not (Array.elem s.source.start.line moved)) copies
          shift = maybe 0 (\s -> s.generated.start.line - s.source.start.line) (Array.head kept)
        assert ("the lines of " <> file <> " shift by at most one (" <> show shift <> ")") (shift == 0 || shift == 1)
        for_ kept \seg ->
          assert
            ("line " <> show (seg.source.start.line + 1) <> " of " <> file <> " stays line " <> show (seg.source.start.line + 1 + shift) <> " (is " <> show (seg.generated.start.line + 1) <> ")")
            (seg.generated.start.line == seg.source.start.line + shift)

-- Scoped styles: each selector also matches the scope's root, unless it
-- names the root already; at-rules that `@scope` cannot hold move out.
css :: Effect Unit
css = do
  let
    cases =
      [ { css: ".a { x: 1 }", expect: "@scope ([data-psx=\"M\"]) {\n  .a, :scope:is(.a) { x: 1 }\n}\n" }
      , { css: ".a::before, :scope > b { x: 1 }", expect: "@scope ([data-psx=\"M\"]) {\n  .a::before, :scope > b, :scope:is(.a)::before { x: 1 }\n}\n" }
      , { css: "a:is(.x, .y):after { x: 1 }", expect: "@scope ([data-psx=\"M\"]) {\n  a:is(.x, .y):after, :scope:is(a:is(.x, .y)):after { x: 1 }\n}\n" }
      , { css: "\n    @media (min-width: 1px) {\n      p { x: 1 }\n    }\n  ", expect: "@scope ([data-psx=\"M\"]) {\n  @media (min-width: 1px) {\n    p, :scope:is(p) { x: 1 }\n  }\n}\n" }
      , { css: "@import \"a.css\";\n@keyframes k { to { x: 1 } }\n[title=\"a,b\"] { x: 1 }", expect: "@import \"a.css\";\n@keyframes k { to { x: 1 } }\n@scope ([data-psx=\"M\"]) {\n  [title=\"a,b\"], :scope:is([title=\"a,b\"]) { x: 1 }\n}\n" }
      ]
  for_ cases \c -> do
    let actual = Css.scoped "M" c.css
    assert ("scoping " <> show c.css <> " gives " <> show actual) (actual == c.expect)

-- A template's lines are kept, its signature takes the blank line before
-- it, and the file's style is stamped on its top-level elements.
psxt :: Effect Unit
psxt = do
  let
    input = String.joinWith "\n"
      [ "<script type=\"text/purescript\">"
      , "module M where"
      , ""
      , "import Portico.Psx (Markup)"
      , ""
      , "foreign import view :: Int -> Markup Unit"
      , "</script>"
      , ""
      , "<template head=\"view n\">"
      , "  <p>{show n}</p>"
      , "</template>"
      , "<style>p { color: red; }</style>"
      , ""
      ]
  case lowerPsxt { sourceName: "M.psxt", generatedName: "M.purs" } input of
    Left errors -> throw ("FAIL the .psxt example lowers: " <> show (map _.message errors))
    Right result -> do
      let lines = String.split (String.Pattern "\n") result.purs
      assert ("the header is line 1: " <> show (Array.index lines 0)) (Array.index lines 0 == Just "-- Generated by psx from M.psxt")
      assert ("the signature is line 8: " <> show (Array.index lines 7)) (Array.index lines 7 == Just "view :: Int -> Markup Unit")
      assert ("the definition is line 9: " <> show (Array.index lines 8)) (Array.index lines 8 == Just "view n =")
      assert ("the paragraph is line 10, stamped: " <> show (Array.index lines 9))
        (map (String.contains (String.Pattern "B.p [ B.attribute \"data-psx\" B.Style.scope ]")) (Array.index lines 9) == Just true)
      case result.style of
        Nothing -> throw "FAIL the .psxt example has a style"
        Just style -> do
          assert "the companion is M.PsxStyle" (style.companion == "M.PsxStyle")
          assert ("the companion imports the stylesheet: " <> show (Css.companionForeign { sourceName: "M.psxt", moduleName: "M", cssImport: "../../src/M.css" } style.scopes))
            ( Css.companionForeign { sourceName: "M.psxt", moduleName: "M", cssImport: "../../src/M.css" } style.scopes
                == "// Generated by psx from M.psxt\nimport \"../../src/M.css\";\n\nexport const scope = \"M\";\n"
            )

attributeWarnings :: Effect Unit
attributeWarnings = do
  let
    input =
      """module W where
import Psx.Static as B
view = <div styleAttr="width: 1px" clas="x" data-value="ok" aria-label="ok"><x-card arbitrary="ok"/><svg viewBox="0 0 1 1"/></div>
"""
  case lower { sourceName: "W.psx", generatedName: "W.purs" } input of
    Left _ -> throw "FAIL warning fixture lowers"
    Right result -> do
      assert "only unknown standard attributes warn" (Array.length result.warnings == 2)
      assert "styleAttr suggests style" (maybe false (String.contains (String.Pattern "did you mean `style`") <<< _.message) (Array.head result.warnings))
      assert "warning points at source attribute" (map (_.range.start.line) (Array.head result.warnings) == Just 2)
      assert "unknown attributes still lower" (String.contains (String.Pattern "B.attribute \"styleAttr\"") result.purs)
