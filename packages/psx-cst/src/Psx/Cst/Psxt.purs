-- | Parses `.psxt` files: an HTML document whose
-- | `<script type="text/purescript">` holds a PureScript module and whose
-- | `<template head="f x y">` elements are the bodies of that module's
-- | `foreign import f :: T` declarations. `head` holds the head of the
-- | equation, the function's name and then PureScript binders, and is copied
-- | into the lowering as written: `<template head="H">…</template>` is the
-- | equation `H = […]`.
-- |
-- | At the top level a file holds the script, templates, `<style>` elements,
-- | comments and whitespace. Every top-level element is scanned as markup,
-- | so the file prints back exactly. The module is parsed from the file's
-- | text with everything outside the script blanked to spaces, so its
-- | positions are the file's.
-- |
-- | Resolution: each template must match a `foreign import` of its name,
-- | which it then defines; a template without one is an error whose fix
-- | inserts `foreign import f :: _` for the author to complete. A
-- | `foreign import` without a template stays real FFI. The words after the
-- | name are PureScript binders, no more of them than the type has arrows.
-- | Several templates with one name are that function's equations, in order:
-- | they follow one another, on the same side of the script, and take the
-- | same number of binders.
module Psx.Cst.Psxt
  ( PsxtResult
  , Psxt
  , Script
  , Template
  , Args
  , ForeignDecl
  , TopItem(..)
  , parsePsxt
  , printPsxt
  , typeArrows
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Psx.Cst.Lexer (scanMarkup)
import Psx.Cst.Parser (parseModule)
import Psx.Cst.Parser.Markup (holeParser, withSource)
import Psx.Cst.Parser.PureScript as Purs
import Psx.Cst.Print (printMarkup)
import Psx.Cst.Source (Source, offsetOf, posOf, source, startsWith)
import Psx.Cst.Types (AttrValue(..), Element, namedAttrs, MToken, Markup(..), Psx, PsxError, mtokenSource)
import PureScript.CST as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Foreign(..), Ident(..), Labeled(..), Module(..), ModuleBody(..), Name(..), SourceRange, Type(..), Wrapped(..))
import PureScript.CST.Types as CSTTypes

type PsxtResult =
  { items :: Array TopItem
  -- | Absent when the file has no script, or its module did not parse.
  , file :: Maybe Psxt
  , errors :: Array PsxError
  }

type Psxt =
  { script :: Script
  , templates :: Array Template
  -- | The top-level `<style>` elements, which apply to every template.
  , styles :: Array (Element Psx)
  }

type Script =
  { element :: Element Psx
  -- | The offsets of the module's text, between `>` and `</script>`.
  , contentStart :: Int
  , contentEnd :: Int
  , module :: Module Psx
  }

type Template =
  { element :: Element Psx
  , name :: String
  , args :: Maybe Args
  -- | Whether this is its function's first equation, which carries the
  -- | signature.
  , first :: Boolean
  -- | The `foreign import` the template defines, if there is one.
  , declaration :: Maybe ForeignDecl
  }

-- | The binders after the name in `head`'s value, and where they are.
type Args =
  { text :: String
  , start :: Int
  , end :: Int
  , binders :: Int
  }

type ForeignDecl =
  { range :: SourceRange
  , type :: CSTTypes.Type Psx
  }

data TopItem
  = TopElement (Element Psx)
  | TopComment MToken
  -- | Whitespace, or text that is an error.
  | TopText MToken

printPsxt :: Array TopItem -> String
printPsxt = foldMap case _ of
  TopElement el -> printMarkup (MarkupElement el)
  TopComment t -> mtokenSource t
  TopText t -> mtokenSource t

parsePsxt :: String -> PsxtResult
parsePsxt text = do
  let
    src = source text
    top = withSource src \_ -> scanTop src
    scripts = Array.filter isScript top.elements
    templates = Array.filter (\el -> el.name.text == "template") top.elements
    styles = Array.filter (\el -> el.name.text == "style") top.elements
  case scripts of
    [ el ] -> do
      let
        start = offsetOf src el.end.range.end
        end = maybe' (SCU.length text) (\c -> offsetOf src c.open.range.start) el.close
        parsed = parseModule (mask start end text)
      case parsed.module of
        Nothing ->
          { items: top.items, file: Nothing, errors: sortErrors (top.errors <> parsed.errors) }
        Just m -> do
          let
            foreigns = foreignValues m
            resolved = map (resolve src el foreigns) templates
            equations = equationErrors src start resolved
          { items: top.items
          , file: Just
              { script: { element: el, contentStart: start, contentEnd: end, module: m }
              , templates: equations.templates
              , styles
              }
          , errors: sortErrors (top.errors <> parsed.errors <> foldMap _.errors resolved <> equations.errors)
          }
    [] ->
      { items: top.items
      , file: Nothing
      , errors: sortErrors
          ( top.errors <>
              [ { range: { start: { line: 0, column: 0 }, end: { line: 0, column: 0 } }
                , message: "A .psxt file holds its module in a <script type=\"text/purescript\">"
                , fix: Nothing
                }
              ]
          )
      }
    _ ->
      { items: top.items
      , file: Nothing
      , errors: sortErrors
          ( top.errors <> (Array.drop 1 scripts <#> \el -> errorAt el "A .psxt file has one <script type=\"text/purescript\">")
          )
      }
  where
  maybe' d f = case _ of
    Just a -> f a
    Nothing -> d

isScript :: Element Psx -> Boolean
isScript el = el.name.text == "script"

errorAt :: Element Psx -> String -> PsxError
errorAt el message = { range: tagRange el, message, fix: Nothing }

-- | `<name`, where an element's errors are reported.
tagRange :: Element Psx -> SourceRange
tagRange el = { start: el.open.range.start, end: el.name.range.end }

sortErrors :: Array PsxError -> Array PsxError
sortErrors = Array.sortWith \e -> [ e.range.start.line, e.range.start.column ]

-- | The text with everything outside the offsets blanked, keeping line
-- | breaks and every UTF-16 code unit's offset.
mask :: Int -> Int -> String -> String
mask start end text = blank (SCU.take start text) <> SCU.slice start end text <> blank (SCU.drop end text)
  where
  blank = Regex.replace (Regex.unsafeRegex "[^\\r\\n]" Regex.global) " "

-- The top level ------------------------------------------------------------------

type Top =
  { items :: Array TopItem
  , elements :: Array (Element Psx)
  , errors :: Array PsxError
  }

scanTop :: Source -> Top
scanTop src = go 0 { items: [], elements: [], errors: [] }
  where
  len = SCU.length src.text

  token from to = { leading: "", text: SCU.slice from to src.text, range: { start: posOf src from, end: posOf src to } }

  go i acc
    | i >= len = acc
    | startsWith src i "<!--" = do
        let
          end = case SCU.indexOf (String.Pattern "-->") (SCU.drop (i + 4) src.text) of
            Just k -> i + 4 + k + 3
            Nothing -> len
          acc' = acc { items = Array.snoc acc.items (TopComment (token i end)) }
        go end
          if end == len && not (startsWith src (len - 3) "-->") then acc' { errors = Array.snoc acc'.errors (rangeError i (i + 4) "Unterminated comment: expected -->") }
          else acc'
    | otherwise = case scanMarkup (holeParser Purs.parseExpr src) src i of
        Just scanned
          | MarkupElement el <- scanned.markup -> do
              let
                holeErrors = scanned.holeErrors <#> \e ->
                  { range: { start: e.position, end: e.position }, message: printParseError e.error, fix: Nothing }
                kind = elementErrors el
              go (max (i + 1) scanned.end)
                { items: Array.snoc acc.items (TopElement el)
                , elements: if Array.null kind then Array.snoc acc.elements el else acc.elements
                , errors: acc.errors <> scanned.errors <> holeErrors <> kind
                }
        _ -> do
          let
            end = textEnd i
            t = token i end
            isBlank = String.trim t.text == ""
          go end
            { items: Array.snoc acc.items (TopText t)
            , elements: acc.elements
            , errors:
                if isBlank then acc.errors
                else Array.snoc acc.errors (rangeError (i + leadingSpace t.text) end "Text at the top of a .psxt file belongs in a <template>")
            }

  -- Text runs to the next `<`, or past a `<` that starts nothing.
  textEnd i = case SCU.indexOf (String.Pattern "<") (SCU.drop (i + 1) src.text) of
    Just k -> i + 1 + k
    Nothing -> len

  leadingSpace t = SCU.length t - SCU.length (SCU.dropWhile isSpace t)

  rangeError from to message = { range: { start: posOf src from, end: posOf src to }, message, fix: Nothing }

  isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

  elementErrors el = case el.name.text of
    "script"
      | attrText "type" el == Just "text/purescript" -> []
      | otherwise -> [ errorAt el "A .psxt file's only <script> is its module: <script type=\"text/purescript\">" ]
    "template" -> []
    "style" -> []
    other -> [ errorAt el ("<" <> other <> "> cannot stand at the top of a .psxt file: it belongs in a <template>") ]

-- | A string attribute's value, without its quotes.
attrText :: String -> Element Psx -> Maybe String
attrText name el = Array.find (\a -> a.name.text == name) (namedAttrs el.attrs) >>= \a -> case a.value of
  Just { value: AttrString t } -> Just (unquote t.text)
  _ -> Nothing

unquote :: String -> String
unquote s = SCU.drop 1 (SCU.dropRight 1 s)

-- Resolution ---------------------------------------------------------------------

type ForeignValue = { name :: String, declaration :: ForeignDecl }

foreignValues :: Module Psx -> Array ForeignValue
foreignValues (Module { body: ModuleBody body }) = Array.mapMaybe go body.decls
  where
  go d = case d of
    CSTTypes.DeclForeign _ _ (ForeignValue (Labeled { label: Name { name: Ident name }, value })) ->
      Just { name, declaration: { range: rangeOf d, type: value } }
    _ -> Nothing

type Resolved = { template :: Template, errors :: Array PsxError }

resolve :: Source -> Element Psx -> Array ForeignValue -> Element Psx -> Resolved
resolve src script foreigns el = do
  let
    nameAttr = Array.find (\a -> a.name.text == "head") (namedAttrs el.attrs)
    argsAttr = Array.find (\a -> a.name.text == "name" || a.name.text == "args") (namedAttrs el.attrs)
    -- `head`'s value: the first word is the name, the rest are binders.
    lhs = nameAttr >>= \a -> case a.value of
      Just { value: AttrString t } -> Just (splitName src (offsetOf src t.range.start + 1) (unquote t.text))
      _ -> Nothing
    name = lhs >>= _.name
    found = name >>= \n -> Array.find (\f -> f.name == n) foreigns
    args = lhs >>= _.args
    binders = args <#> \a -> countBinders a.text
    nameErrors = case nameAttr, name of
      -- `name` or `args` in place of `head` is reported once, below.
      Nothing, _
        | isJust argsAttr -> []
        | otherwise -> [ errorAt el "A <template> needs a head: <template head=\"f\">, for its module's foreign import f, or head=\"f x y\" with its arguments" ]
      Just a, Nothing -> [ { range: a.name.range, message: "A template's head is a string: head=\"f\", or head=\"f x y\" with its arguments", fix: Nothing } ]
      _, Just n
        | isNothing found ->
            [ { range: maybe' (tagRange el) (\a -> attrRange a) nameAttr
              , message: "<template head=\"" <> n <> "\"> has no foreign import " <> n <> " :: T in the script to define"
              , fix: Just { message: "Declare foreign import " <> n <> " :: _", edits: [ insertForeign n ] }
              }
            ]
        | otherwise -> []
    argsErrors = case argsAttr, args, binders of
      Just a, _, _ -> [ { range: attrRange a, message: "A template's name and arguments are the head of its equation: head=\"f x y\" rather than " <> a.name.text, fix: Nothing } ]
      _, Just a, Just Nothing -> [ { range: a.range, message: "The words after a template's name are PureScript binders, as in the head of an equation: head=\"f { title } (Just n)\"", fix: Nothing } ]
      _, Just a, Just (Just n)
        | Just f <- found
        , not (endsInWildcard f.declaration.type)
        , n > typeArrows f.declaration.type ->
            [ { range: a.range
              , message: "<template head=\"" <> f.name <> " " <> a.text <> "\"> has " <> count n "binder" <> " but the type of " <> f.name <> " has " <> count (typeArrows f.declaration.type) "arrow"
              , fix: Nothing
              }
            ]
      _, _, _ -> []
  { template:
      { element: el
      , name: fromMaybe "" name
      , args: args <#> \a -> { text: a.text, start: a.start, end: a.end, binders: fromMaybe 0 (join binders) }
      , first: true
      , declaration: _.declaration <$> found
      }
  , errors: nameErrors <> argsErrors
  }
  where
  maybe' d f = case _ of
    Just a -> f a
    Nothing -> d

  attrRange a = case a.value of
    Just { value: AttrString t } -> { start: a.name.range.start, end: t.range.end }
    _ -> a.name.range

  count n noun = show n <> " " <> noun <> if n == 1 then "" else "s"

  -- Inserted on a line of its own just before `</script>`.
  insertForeign n = do
    let
      at = case script.close of
        Just c -> c.open.range.start
        Nothing -> posOf src (SCU.length src.text)
      text = "foreign import " <> n <> " :: _\n"
    { range: { start: at, end: at }, text: if at.column == 0 then text else "\n" <> text }

-- | Several templates with one name are the equations of one function. Each
-- | after the first must follow one with the same name, on the same side of
-- | the script, and take as many binders; it is then not the first.
equationErrors :: Source -> Int -> Array Resolved -> { templates :: Array Template, errors :: Array PsxError }
equationErrors src scriptStart resolved =
  { templates: Array.mapWithIndex (\i r -> r.template { first = isNothing (firstOf i r) }) resolved
  , errors: Array.catMaybes (Array.mapWithIndex check resolved)
  }
  where
  name r = r.template.name
  firstOf i r
    | name r == "" = Nothing
    | otherwise = Array.find (\p -> name p == name r) (Array.take i resolved)
  beforeScript r = offsetOf src r.template.element.open.range.start < scriptStart
  -- Nothing when the binders don't parse, which is already an error.
  arity r = case r.template.args of
    Nothing -> Just 0
    Just a
      | a.binders == 0 -> Nothing
      | otherwise -> Just a.binders
  check i r = firstOf i r >>= \f -> case Array.index resolved (i - 1), arity f, arity r of
    Just p, _, _
      | name p /= name r || beforeScript p /= beforeScript r ->
          Just (errorAt r.template.element ("This <template head=\"" <> name r <> " …\"> is another equation of " <> name r <> ", so it goes right after " <> name r <> "'s other templates"))
    _, Just 0, Just 0 ->
      Just (errorAt r.template.element ("A second <template head=\"" <> name r <> "\">: " <> name r <> " has no arguments, so it has one equation"))
    _, Just m, Just n
      | m /= n ->
          Just (errorAt r.template.element ("This equation of " <> name r <> " has " <> count n <> " but its first has " <> count m <> "; every equation takes the same number"))
    _, _, _ -> Nothing
  count n = show n <> " binder" <> if n == 1 then "" else "s"

-- | Splits `head`'s value, which starts at `start`, into the template's name
-- | and the binders after it, with where they are.
splitName :: Source -> Int -> String -> { name :: Maybe String, args :: Maybe { text :: String, start :: Int, end :: Int, range :: SourceRange } }
splitName src start value = do
  let
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    lead = SCU.length (SCU.takeWhile isSpace value)
    afterLead = SCU.drop lead value
    word = SCU.takeWhile (not <<< isSpace) afterLead
    rest = SCU.drop (SCU.length word) afterLead
    gap = SCU.length (SCU.takeWhile isSpace rest)
    text = String.trim rest
    argsStart = start + lead + SCU.length word + gap
    argsEnd = argsStart + SCU.length text
  { name: if word == "" then Nothing else Just word
  , args: if text == "" then Nothing else Just { text, start: argsStart, end: argsEnd, range: { start: posOf src argsStart, end: posOf src argsEnd } }
  }

-- | The number of binders, or nothing if they don't parse as binders alone.
countBinders :: String -> Maybe Int
countBinders args = case CST.parseDecl ("f " <> args <> " = f") of
  CST.ParseSucceeded (CSTTypes.DeclValue v@{ guarded: CSTTypes.Unconditional _ _ }) -> Just (Array.length v.binders)
  _ -> Nothing

-- | Whether the type ends in `_` or `?hole`, as the quick fix leaves it, so
-- | its arrows are not yet known.
endsInWildcard :: forall e. CSTTypes.Type e -> Boolean
endsInWildcard = case _ of
  TypeForall _ _ _ t -> endsInWildcard t
  TypeConstrained _ _ t -> endsInWildcard t
  TypeArrow _ _ t -> endsInWildcard t
  TypeParens (Wrapped w) -> endsInWildcard w.value
  TypeWildcard _ -> true
  TypeHole _ -> true
  _ -> false

-- | The arrows of a type, under its `forall`s and constraints.
typeArrows :: forall e. CSTTypes.Type e -> Int
typeArrows = case _ of
  TypeForall _ _ _ t -> typeArrows t
  TypeConstrained _ _ t -> typeArrows t
  TypeArrow _ _ t -> 1 + typeArrows t
  TypeParens (Wrapped w) -> typeArrows w.value
  _ -> 0
