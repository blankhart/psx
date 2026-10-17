-- | The parse stage: `expected.parse` records whether printing the parsed
-- | module gives back the input, the errors with their fixes, and an outline
-- | of every markup literal. For a `.psxt` file it outlines the script, each
-- | template with the foreign import it defines, and the styles. Positions
-- | are `line:column`, counted from 1.
module Psx.Corpus.Parse (parse, parsePsxt, outline, outlinePsxt) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap, intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Psx.Corpus (Stage)
import Psx.Cst (AttrValue(..), Child(..), Hole, Markup(..), MarkupRoot, Psx, PsxError, foldMapHoles, foldMapMarkupRootsExpr, parseModule, printExpr, printModule)
import Psx.Cst.Psxt as Psxt
import Psx.Cst.Traversal (foldMapMarkupRoots)
import PureScript.CST.Types (Module, SourcePos, SourceRange)

parse :: Stage
parse =
  { name: "parse"
  , golden: "expected.parse"
  , applies: \c -> c.inputFile == "input.psx"
  , run: \c -> pure (outline c.input)
  }

parsePsxt :: Stage
parsePsxt =
  { name: "parse"
  , golden: "expected.parse"
  , applies: \c -> c.inputFile == "input.psxt"
  , run: \c -> pure (outlinePsxt c.input)
  }

outlinePsxt :: String -> String
outlinePsxt input = do
  let result = Psxt.parsePsxt input
  unlines $
    [ if Psxt.printPsxt result.items == input then "print: identical" else "print: DIFFERS" ]
      <> errors result.errors
      <> case result.file of
        Nothing -> [ "file: none" ]
        Just file ->
          [ "script: " <> pos file.script.element.open.range.start <> "-" <> pos (endOf file.script.element)
              <> ", module "
              <> String.trim (String.takeWhile (_ /= String.codePointFromChar '\n') (String.drop 7 (moduleText file)))
          ]
            <> foldMap template file.templates
            <> map (\st -> "style " <> pos st.open.range.start) file.styles
  where
  moduleText file = String.trim (String.drop file.script.contentStart (String.take file.script.contentEnd input))
    # \t -> String.drop (maybe 0 identity (String.indexOf (String.Pattern "module") t)) t
  endOf el = case el.close of
    Just c -> maybe c.name.range.end _.range.end c.end
    Nothing -> el.end.range.end
  template t =
    [ "template " <> show t.name <> " at " <> pos t.element.open.range.start
        <> foldMap (\a -> ", args " <> show a.text <> " (" <> show a.binders <> " binders)") t.args
        <> case t.declaration of
          Just d -> ", defines the foreign import at " <> pos d.range.start <> " (" <> show (Psxt.typeArrows d.type) <> " arrows)"
          Nothing -> ", no foreign import"
    ]
      <> foldMap (child 1) t.element.children

outline :: String -> String
outline input = do
  let result = parseModule input
  unlines $
    [ case result.module of
        Nothing -> "print: no tree (the parse failed)"
        Just m
          | printModule m == input -> "print: identical"
          | otherwise -> "print: DIFFERS"
    ]
      <> errors result.errors
      <> maybe [] markup result.module

unlines :: Array String -> String
unlines ls = intercalate "\n" ls <> "\n"

errors :: Array PsxError -> Array String
errors [] = [ "errors: none" ]
errors es = [ "errors:" ] <> foldMap error es
  where
  error e =
    [ "  " <> range e.range <> " " <> e.message ]
      <> foldMap (\f -> [ "    fix: " <> f.message <> ": " <> intercalate ", " (map edit f.edits) ]) e.fix
  edit ed
    | ed.range.start == ed.range.end = "insert " <> show ed.text <> " at " <> pos ed.range.start
    | otherwise = "replace " <> range ed.range <> " with " <> show ed.text

markup :: Module Psx -> Array String
markup m = case foldMapMarkupRoots (\r -> [ r ]) m of
  [] -> [ "markup: none" ]
  _ -> [ "markup:" ] <> topLevel
  where
  -- Roots in holes are printed under their hole, so list only the
  -- outermost ones here.
  topLevel = foldMapMarkupRoots (\r -> [ r ]) m
    # Array.filter (\r -> not (Array.elem (key r) nestedKeys))
    # foldMap (root 1)
  nestedKeys = foldMapMarkupRoots (\r -> foldMapHoles (\h -> map key (foldMapMarkupRootsExpr (\r' -> [ r' ]) h.expr)) r.markup) m
  key r = pos (rangeStart r.markup)

rangeStart :: Markup Psx -> SourcePos
rangeStart = case _ of
  MarkupElement el -> el.open.range.start
  MarkupFragment fr -> fr.open.range.start

root :: Int -> MarkupRoot -> Array String
root depth r = node depth r.markup

node :: Int -> Markup Psx -> Array String
node depth = case _ of
  MarkupElement el -> do
    let
      notes = Array.catMaybes
        [ if el.end.text == "/>" then Just "self-closing" else Nothing
        , if el.end.text == ">" && Array.null el.children && el.close == Nothing then Just "no close" else Nothing
        ]
    [ indent depth <> pos el.open.range.start <> " <" <> el.name.text <> ">" <> foldMap (\n -> " (" <> n <> ")") notes ]
      <> foldMap attr el.attrs
      <> foldMap (child (depth + 1)) el.children
  MarkupFragment fr ->
    [ indent depth <> pos fr.open.range.start <> " <>" <> if fr.close == Nothing then " (no close)" else "" ]
      <> foldMap (child (depth + 1)) fr.children
  where
  attr a = [ indent (depth + 1) <> "@" <> a.name.text <> foldMap value a.value ] <> foldMap nested a.value
    where
    value v = "=" <> case v.value of
      AttrString t -> t.text
      AttrHole h -> holeText h
      AttrRaw t -> "raw " <> show t.text
    nested v = case v.value of
      AttrHole h -> holeRoots (depth + 2) h
      _ -> []

child :: Int -> Child Psx -> Array String
child depth = case _ of
  ChildText t -> [ indent depth <> "text " <> show t.text ]
  ChildMarkup m -> node depth m
  ChildComment t -> [ indent depth <> "comment " <> show t.text ]
  ChildHole h -> [ indent depth <> "hole " <> holeText h ] <> holeRoots (depth + 1) h
  ChildRaw t -> [ indent depth <> "raw " <> show t.text ]

holeText :: Hole Psx -> String
holeText h = "{" <> oneLine (printExpr h.expr) <> "}"

holeRoots :: Int -> Hole Psx -> Array String
holeRoots depth h = foldMapMarkupRootsExpr (\r -> [ r ]) h.expr
  # Array.filter (\r -> not (Array.elem (pos (rangeStart r.markup)) inner))
  # foldMap (root depth)
  where
  inner = foldMapMarkupRootsExpr (\r -> foldMapHoles (\h' -> map (pos <<< rangeStart <<< _.markup) (foldMapMarkupRootsExpr (\r' -> [ r' ]) h'.expr)) r.markup) h.expr

oneLine :: String -> String
oneLine = String.trim <<< Regex.replace (Regex.unsafeRegex "\\s*\\n\\s*" Regex.global) " "

indent :: Int -> String
indent n = String.joinWith "" (Array.replicate n "  ")

pos :: SourcePos -> String
pos p = show (p.line + 1) <> ":" <> show (p.column + 1)

range :: SourceRange -> String
range r = pos r.start <> "-" <> pos r.end
