module Test.Psx.Format where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (match) as Regex
import Data.String.Regex.Flags (global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Psx.Cst (Attr(..), AttrValue(..), Child(..), Markup(..), MarkupRoot, Psx, foldMapMarkupRoots, parseModule, printModule)
import Psx.Cst.Psxt (TopItem(..), parsePsxt, printPsxt)
import Psx.Format (formatPsx, formatPsxt)
import Psx.Lower (lower, lowerPsxt)
import Psx.Lower.Whitespace (normalise)

main :: Effect Unit
main = do
  layouts
  files <- sources
  for_ files properties
  log $ "psx-fmt: all tests passed (" <> show (Array.length files) <> " files)"

assert :: String -> Boolean -> Effect Unit
assert what ok = unless ok (throw ("FAIL " <> what))

-- Layouts ---------------------------------------------------------------------

layouts :: Effect Unit
layouts = do
  let
    m body = "module M where\n\nimport Portico.Psx (Markup)\n\nx :: Markup Unit\nx =" <> body
    same what body = formats what (m body) (m body)
  same "a short element stays on its line" " <p class=\"a\">Hello {name}!</p>\n"
  formats "an element with element children breaks"
    (m " <ul><li>one</li><li>two</li></ul>\n")
    (m "\n  <ul>\n    <li>one</li>\n    <li>two</li>\n  </ul>\n")
  formats "whitespace that renders stays; whitespace that doesn't goes"
    (m "\n  <p>\n    Hello,\n      {name}!   How\n    are you?\n\n    Fine.\n    <b> bold </b> <i>italic</i>\n  </p>\n")
    (m "\n  <p>\n    Hello,\n    {name}!   How are you? Fine.\n    <b> bold </b> <i>italic</i>\n  </p>\n")
  formats "text fills lines"
    (m " <p>" <> words 30 <> "</p>\n")
    (m "\n  <p>\n    " <> words 15 <> "\n    " <> words 15 <> "\n  </p>\n")
  formats "attributes that do not fit go one per line"
    (m " <input class=\"form-control form-control-lg\" id=\"name-input\" placeholder=\"Your name\" required/>\n")
    (m "\n  <input\n    class=\"form-control form-control-lg\"\n    id=\"name-input\"\n    placeholder=\"Your name\"\n    required\n  />\n")
  formats "markup as an argument gets parentheses"
    (m " f <p/>\n")
    (m " f (<p/>)\n")
  formats "a hole is laid out by purs-tidy"
    (m " <p>{ show   n }</p>\n")
    (m " <p>{show n}</p>\n")
  formats "a stylesheet is reindented"
    (m "\n  <div>\n        <style>\n          p { margin: 0; }\n            a { color: red; }\n        </style>\n  </div>\n")
    (m "\n  <div>\n    <style>\n      p { margin: 0; }\n        a { color: red; }\n    </style>\n  </div>\n")
  case formatPsx (m " <p>\n") of
    Left _ -> pure unit
    Right _ -> throw "FAIL a file with errors is not formatted"
  where
  words n = String.drop 1 (String.joinWith "" (Array.replicate n " word"))

formats :: String -> String -> String -> Effect Unit
formats what input expected = case formatPsx input of
  Left errors -> throw ("FAIL " <> what <> ": " <> String.joinWith "; " (map _.message errors))
  Right actual -> assert (what <> ":\n" <> actual <> "expected:\n" <> expected) (actual == expected)

-- Properties ------------------------------------------------------------------

-- | On every file: formatting succeeds when the file parses without errors,
-- | formatting the result changes nothing, the result parses back to the same
-- | markup, and it renders the same text.
properties :: FilePath -> Effect Unit
properties file = do
  input <- FS.readTextFile UTF8 file
  let
    isPsxt = Path.extname file == ".psxt"
    format = if isPsxt then formatPsxt else formatPsx
    parsesClean = if isPsxt then Array.null (parsePsxt input).errors else Array.null (parseModule input).errors
  case format input of
    Left errors -> assert (file <> " formats: " <> String.joinWith "; " (map _.message errors)) (not parsesClean)
    Right formatted -> do
      assert (file <> " formats to itself the second time") (format formatted == Right formatted)
      roundTrips file isPsxt input formatted
      sameText file isPsxt input formatted

roundTrips :: FilePath -> Boolean -> String -> String -> Effect Unit
roundTrips file isPsxt input formatted = do
  let
    outline text
      | isPsxt = do
          let result = parsePsxt text
          { clean: Array.null result.errors
          , printed: printPsxt result.items == text
          , markup: result.items # foldMap case _ of
              TopElement el
                | el.name.text == "script" -> []
                | otherwise -> markupOutline (MarkupElement el)
              _ -> []
          , roots: foldMap (\f -> foldMapMarkupRoots rootOutline f.script.module) result.file
          }
      | otherwise = do
          let result = parseModule text
          { clean: Array.null result.errors
          , printed: map printModule result.module == Just text
          , markup: []
          , roots: foldMap (foldMapMarkupRoots rootOutline) result.module
          }
    before = outline input
    after = outline formatted
  assert (file <> " formatted parses without errors") after.clean
  assert (file <> " formatted prints back exactly") after.printed
  -- A file with errors (markup as an argument) is compared once fixed.
  when before.clean do
    assert (file <> " formatted has the same markup") (before.markup == after.markup && before.roots == after.roots)

rootOutline :: MarkupRoot -> Array String
rootOutline root = markupOutline root.markup

-- | The markup's tags, attributes and text as the policy renders it; holes
-- | and comments only as markers, since their layout changes.
markupOutline :: Markup Psx -> Array String
markupOutline = case _ of
  MarkupElement el ->
    [ "<" <> el.name.text ]
      <> map attr el.attrs
      <> (if el.name.text == "style" then [] else foldMap child el.children)
      <> [ ">" ]
  MarkupFragment fr -> [ "<>" ] <> foldMap child fr.children <> [ "</>" ]
  where
  attr = case _ of
    AttrNamed a -> "@" <> a.name.text <> foldMap value a.value
    AttrTrait _ -> "@{}"
  value v = case v.value of
    AttrString t -> "=" <> t.text
    AttrHole _ -> "={}"
    AttrRaw t -> "=raw " <> t.text
  child = case _ of
    ChildText t -> let n = normalise t.text in if n == "" then [] else [ show n ]
    ChildMarkup m -> markupOutline m
    ChildComment _ -> [ "comment" ]
    ChildHole _ -> [ "{}" ]
    ChildRaw t -> [ "raw " <> t.text ]

-- | Where the file lowers, the formatted file lowers to the same text nodes.
sameText :: FilePath -> Boolean -> String -> String -> Effect Unit
sameText file isPsxt input formatted = do
  let
    names = { sourceName: "input", generatedName: "input.purs" }
    lowerText text = (if isPsxt then lowerPsxt names text else lower names text) <#> \l -> textNodes l.purs
  case lowerText input of
    Left _ -> pure unit
    Right before -> case lowerText formatted of
      Left _ -> throw ("FAIL " <> file <> " formatted does not lower")
      Right after -> assert (file <> " formatted renders the same text: " <> show before <> " /= " <> show after) (before == after)

textNodes :: String -> Array String
textNodes purs = case Regex.match (Regex.unsafeRegex "B\\.text \"(?:[^\"\\\\]|\\\\.)*\"" Regex.global) purs of
  Just found -> Array.catMaybes (Array.fromFoldable found)
  Nothing -> []

-- Files -----------------------------------------------------------------------

-- | Every `.psx` and `.psxt` file in the corpus, the examples and the suite.
sources :: Effect (Array FilePath)
sources = do
  root <- workspaceRoot
  Array.sort <<< Array.concat <$> for [ "test/corpus", "examples", "test/suite/apps" ] \dir ->
    walk (Path.concat [ root, dir ])
  where
  walk path = do
    stats <- FS.stat path
    if Stats.isDirectory stats then do
      names <- FS.readdir path
      let keep n = not (String.take 1 n == "." || Array.elem n [ "node_modules", "output", ".spago" ])
      Array.concat <$> for (Array.filter keep names) \n -> walk (Path.concat [ path, n ])
    else pure (if Path.extname path == ".psx" || Path.extname path == ".psxt" then [ path ] else [])

workspaceRoot :: Effect FilePath
workspaceRoot = Process.cwd >>= go
  where
  go dir = do
    found <- FS.exists (Path.concat [ dir, "test", "corpus" ])
    if found then pure dir
    else if Path.dirname dir == dir then throw "no workspace root above the current directory"
    else go (Path.dirname dir)
