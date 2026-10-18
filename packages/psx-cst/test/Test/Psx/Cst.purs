module Test.Psx.Cst where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
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
import Psx.Cst (Child(..), Markup(..), Psx, foldMapMarkupRoots, parseModule, printModule)
import Psx.Cst.Psxt (parsePsxt, printPsxt)
import Psx.Cst.Source (offsetOf, posOf, source)
import PureScript.CST.Range (rangeOf)
import PureScript.CST as CST
import PureScript.CST.Types (Module)

main :: Effect Unit
main = do
  positions
  operators
  corpus
  psxt
  log "psx-cst: all tests passed"

assert :: String -> Boolean -> Effect Unit
assert what ok = unless ok (throw ("FAIL " <> what))

-- Columns count code points, as cst-parser's do; offsets count UTF-16 units.
positions :: Effect Unit
positions = do
  let src = source "ab\n🎉x\r\nyz"
  assert "position after an astral character" (posOf src 5 == { line: 1, column: 1 })
  assert "offset of that position" (offsetOf src { line: 1, column: 1 } == 5)
  assert "position on a CRLF line" (posOf src 9 == { line: 2, column: 1 })
  assert "offset round trip" (Array.all (\i -> offsetOf src (posOf src i) == i) [ 0, 1, 2, 3, 5, 6, 8, 9, 10 ])

-- `<` keeps its meaning wherever it isn't markup, and the tree there is
-- cst-parser's.
operators :: Effect Unit
operators = do
  for_
    [ "module M where\nx = a <b\n"
    , "module M where\nx = (<) a b\n"
    , "module M where\nx = a <> b\ny = (<>)\n"
    , "module M where\nx = a <$> b\n  <*> c\n"
    , "module M where\nx = do\n  a\n  <#> f\n"
    ]
    \input -> do
      let result = parseModule input
      assert ("no errors in " <> show input) (Array.null result.errors)
      case result.module, CST.parseModule input of
        Just m, CST.ParseSucceeded m' -> do
          assert ("no markup in " <> show input) (Array.null (roots m))
          assert ("same tree as cst-parser for " <> show input) (sameTree m m')
        _, _ -> throw ("FAIL parse of " <> show input)
  -- The comparison itself tells trees apart.
  case (parseModule "module M where\nx = 1\n").module, CST.parseModule "module M where\nx = 2\n" of
    Just m, CST.ParseSucceeded m' -> assert "different trees differ" (not (sameTree m m'))
    _, _ -> throw "FAIL parse of x = 1"
  -- A do statement may be markup, even at the block's column.
  let doBlock = parseModule "module M where\nx = do\n  a <- b\n  <p/>\n"
  assert "markup as a do statement" (Array.null doBlock.errors)
  -- Text keeps comment-like content and everything after a hole.
  case (parseModule "module M where\nx = <p>{a} -- b</p>\n").module of
    Just m -> case roots m of
      [ MarkupElement el ] -> assert "text after a hole" case el.children of
        [ _, ChildText t ] -> t.text == " -- b"
        _ -> false
      _ -> throw "FAIL one root expected"
    Nothing -> throw "FAIL text after a hole"
  where
  roots = foldMapMarkupRoots (\r -> [ r.markup ])

foreign import sameTreeImpl :: forall a b. a -> b -> Boolean

sameTree :: forall a. Module Psx -> Module a -> Boolean
sameTree = sameTreeImpl

-- Print-after-parse is the identity on every corpus case.
corpus :: Effect Unit
corpus = do
  dir <- Process.cwd
  findCorpus dir >>= case _ of
    Nothing -> throw "FAIL no test/corpus directory above the current directory"
    Just root -> do
      all <- walk root
      let
        files = Array.filter (\f -> Path.basename f == "input.psx") all
        psxtFiles = Array.filter (\f -> Path.basename f == "input.psxt") all
      assert "corpus has .psx cases" (not (Array.null files))
      assert "corpus has .psxt cases" (not (Array.null psxtFiles))
      for_ files \file -> do
        input <- FS.readTextFile UTF8 file
        case (parseModule input).module of
          Just m -> assert ("print-after-parse is the identity on " <> file) (printModule m == input)
          Nothing -> pure unit
      for_ psxtFiles \file -> do
        input <- FS.readTextFile UTF8 file
        assert ("print-after-parse is the identity on " <> file) (printPsxt (parsePsxt input).items == input)

-- A .psxt file's module keeps the file's positions, and a template without a
-- foreign import gets a fix that declares one.
psxt :: Effect Unit
psxt = do
  let
    input = "<!-- x -->\n<script type=\"text/purescript\">\nmodule M where\n\nimport Portico.Psx (Markup)\n\nforeign import a :: Markup Unit\n</script>\n<template name=\"a\"><p/></template>\n<template name=\"b x y\"><p>{x}</p></template>\n"
    result = parsePsxt input
  case result.file of
    Nothing -> throw "FAIL the .psxt example parses"
    Just file -> do
      let start = (rangeOf file.script.module).start
      assert ("the module starts at 3:1, not " <> show start) (start == { line: 2, column: 0 })
      assert "both templates are found" (map _.name file.templates == [ "a", "b" ])
      assert "a defines its foreign import" (map (map (_.range.start.line) <<< _.declaration) file.templates == [ Just 6, Nothing ])
  case result.errors of
    [ e ] | Just fix <- e.fix, [ edit ] <- fix.edits -> do
      let
        src = source input
        at = offsetOf src edit.range.start
        fixed = String.take at input <> edit.text <> String.drop at input
      assert ("the fix declares b: " <> show edit.text) (edit.text == "foreign import b :: _\n")
      assert "the fixed file resolves" (Array.null (parsePsxt fixed).errors)
    errors -> throw ("FAIL one error with a fix, not " <> show (map _.message errors))

findCorpus :: FilePath -> Effect (Maybe FilePath)
findCorpus dir = do
  let candidate = Path.concat [ dir, "test", "corpus" ]
  found <- FS.exists candidate
  if found then pure (Just candidate)
  else do
    let parent = Path.dirname dir
    if parent == dir then pure Nothing else findCorpus parent

walk :: FilePath -> Effect (Array FilePath)
walk dir = do
  names <- Array.sort <$> FS.readdir dir
  Array.concat <$> for names \n -> do
    let p = Path.concat [ dir, n ]
    isDir <- Stats.isDirectory <$> FS.stat p
    if isDir then walk p else pure [ p ]
