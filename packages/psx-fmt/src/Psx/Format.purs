-- | Formats mixed markup and PureScript, with configurable purs-tidy layout.
-- |
-- | Markup is purs-tidy's error payload, so purs-tidy places a literal as it
-- | places any expression, and this module lays out the literal itself on
-- | an 80-column page, holes included:
-- |
-- | - An element fits on one line, or its children go on their own lines,
-- |   indented, between the tags on lines of their own. Children always do
-- |   when one of them is an element, a fragment or a comment, or a hole
-- |   that takes several lines.
-- | - Attributes stay on the tag's line if they fit, or go one per line with
-- |   the `>` or `/>` on a line of its own.
-- | - Text is reflowed: words fill lines to the page width. A line breaks
-- |   only where the whitespace policy makes the break mean what was there:
-- |   between words, between two tags, and where the source has a line
-- |   break between a tag or hole and what follows it. Whitespace that
-- |   renders as a space next to a tag or hole stays on its line.
-- | - A hole is `{expr}`, its expression laid out by purs-tidy; when that
-- |   does not fit on the line, the expression goes on lines of its own,
-- |   indented, between the braces.
-- | - `<style>`'s stylesheet is reindented under its element; `<script>`'s
-- |   non-PureScript content and comments keep their text.
-- | - Markup used as a function argument gets the parentheses it needs.
-- |
-- | A file with any other error is not formatted.
module Psx.Format
  ( formatPsx
  , formatPsxt
  , formatPsxWith
  , formatPsxtWith
  , pageWidth
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldMap, foldr)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo (Doc)
import Dodo as Dodo
import Psx.Format.Config as Config
import Psx.Cst (Attr(..), AttrValue(..), NamedAttr, Child(..), Element, Hole, Markup(..), Psx(..), PsxError, parseModule)
import Psx.Cst.Psxt (TopItem(..), parsePsxt)
import Psx.Cst.Source (offsetOf, source)
import Psx.Lower.Whitespace (isWhitespace, normalise, trimEnd, trimStart)
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Comment(..), LineFeed, Module, Token(..))
import PureScript.CST.Types as CST
import Tidy (FormatOptions, ImportSortOption(..), UnicodeOption(..), formatExpr, formatModule)
import Tidy as Tidy
import Tidy.Doc (FormatDoc(..))
import Tidy.Doc as TDoc
import Tidy.Operators (parseOperatorTable)
import Tidy.Operators.Defaults (defaultOperators)
import Tidy.Precedence (PrecedenceMap, remapOperators)

pageWidth :: Int
pageWidth = 80

-- | Formats a `.psx` module, or gives the errors that stop it.
formatPsx :: String -> Either (Array PsxError) String
formatPsx = formatPsxWith fallback

fallback :: ConfigData
fallback = { settings: Config.defaults { importSort = ImportSortIde, unicode = UnicodeNever }, operators: defaultOperators }

formatPsxWith :: ConfigData -> String -> Either (Array PsxError) String
formatPsxWith config input = do
  m <- parsed (parseModule input) (parseModule <<< applyFixes input)
  pure (printModuleText config m <> "\n")
  where
  parsed result reparse = case result.module of
    Just m | Array.null result.errors -> Right m
    _
      | onlyArgumentErrors result.errors
      , fixed <- reparse result.errors
      , Just m <- fixed.module
      , Array.null fixed.errors -> Right m
      | otherwise -> Left result.errors

-- | The module laid out by purs-tidy, without blank lines around it.
printModuleText :: ConfigData -> Module Psx -> String
printModuleText config m = trimLines (printWith config (TDoc.toDoc (formatModule (optionsWith config (remapOperators (parseOperatorTable config.operators) m)) m)))

-- | Formats a `.psxt` file, or gives the errors that stop it.
formatPsxt :: String -> Either (Array PsxError) String
formatPsxt = formatPsxtWith fallback

formatPsxtWith :: ConfigData -> String -> Either (Array PsxError) String
formatPsxtWith config input0 = do
  input <- fixed
  let result = parsePsxt input
  case result.file of
    Just file -> do
      let
        conf = optionsWith config (remapOperators (parseOperatorTable config.operators) file.script.module)
        items = Array.mapMaybe (topItem conf file.script.module) result.items
      pure (joinTop items <> "\n")
    Nothing -> Left result.errors
  where
  fixed = do
    let result = parsePsxt input0
    if Array.null result.errors then Right input0
    else if onlyArgumentErrors result.errors then do
      let input = applyFixes input0 result.errors
      if Array.null (parsePsxt input).errors then Right input else Left result.errors
    else Left result.errors

  -- Each top-level item as text, and the number of line breaks before it.
  topItem conf m = case _ of
    TopElement el
      | el.name.text == "script" ->
          Just { text: print (onPage (openTag conf el).doc) <> "\n" <> printModuleText config m <> "\n</script>", comment: false, breaks: 0 }
      | otherwise ->
          Just { text: print (onPage (markup conf (MarkupElement el)).doc), comment: false, breaks: 0 }
    TopComment t ->
      Just { text: print (verbatimRelative t.range.start.column t.text), comment: true, breaks: 0 }
    TopText t ->
      Just { text: "", comment: false, breaks: newlines t.text }

  -- Top-level items are separated by a blank line, except that a comment
  -- directly above an item stays attached to it.
  joinTop items = go "" Nothing 2 items
    where
    go acc prev breaks xs = case Array.uncons xs of
      Nothing -> acc
      Just { head, tail }
        | head.text == "" -> go acc prev (breaks + head.breaks) tail
        | otherwise -> do
            let
              sep = case prev of
                Nothing -> ""
                Just p
                  | p.comment && breaks < 2 -> "\n"
                  | otherwise -> "\n\n"
            go (acc <> sep <> head.text) (Just head) 0 tail

  -- The count of line breaks in some whitespace.
  newlines s = Array.length (Array.filter (_ == '\n') (SCU.toCharArray s))

-- Fixes ---------------------------------------------------------------------

argumentMessage :: String
argumentMessage = "Markup as a function argument must be parenthesised"

onlyArgumentErrors :: Array PsxError -> Boolean
onlyArgumentErrors errors = not (Array.null errors) && Array.all (\e -> e.message == argumentMessage && isJust e.fix) errors

-- | The source with every error's fix applied.
applyFixes :: String -> Array PsxError -> String
applyFixes input errors = do
  let
    src = source input
    edits = errors # foldMap (foldMap _.edits <<< _.fix) # map \e ->
      { start: offsetOf src e.range.start, end: offsetOf src e.range.end, text: e.text }
  -- Later edits first, so earlier offsets stay put.
  Array.foldl (\text e -> SCU.take e.start text <> e.text <> SCU.drop e.end text) input
    (Array.reverse (Array.sortWith (\e -> [ e.start, e.end ]) edits))

-- purs-tidy -------------------------------------------------------------------

optionsWith :: forall a. ConfigData -> PrecedenceMap -> FormatOptions Psx a
optionsWith config operators =
  { formatError: \e -> formatPayload (optionsWith config operators) e
  , unicode: config.settings.unicode
  , typeArrowPlacement: config.settings.typeArrowPlacement
  , operators
  , importSort: config.settings.importSort
  , importWrap: config.settings.importWrap
  }

type ConfigData = { settings :: Config.FormatOptions, operators :: Array String }

printWith :: forall a. ConfigData -> Doc a -> String
printWith config = Dodo.print Dodo.plainText
  { pageWidth: fromMaybe top config.settings.width
  , ribbonRatio: config.settings.ribbon
  , indentWidth: config.settings.indent
  , indentUnit: String.joinWith "" (Array.replicate config.settings.indent " ")
  }

-- | Markup sets its own width.
print :: forall a. Doc a -> String
print = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = top, ribbonRatio = 1.0 })

onPage :: forall a. Doc a -> Doc a
onPage = Dodo.locally (_ { pageWidth = pageWidth })

-- | Without blank lines at the start and end, or spaces at line ends.
trimLines :: String -> String
trimLines text = do
  let
    lines = map trimEnd (String.split (String.Pattern "\n") text)
    blank = eq ""
  String.joinWith "\n" (Array.reverse (Array.dropWhile blank (Array.reverse (Array.dropWhile blank lines))))

formatPayload :: forall a. FormatOptions Psx a -> Psx -> FormatDoc a
formatPayload conf = case _ of
  PsxRecovered err -> Tidy.formatError err
  PsxMarkup root -> do
    let r = markup conf root.markup
    foldr leadingComment (fromLaidOut r { doc = onPage r.doc }) root.leadingComments

fromLaidOut :: forall a. LaidOut a -> FormatDoc a
fromLaidOut r = case TDoc.fromDoc r.doc of
  FormatDoc d -> FormatDoc d { multiline = r.multiline }

-- | A comment before the markup, as purs-tidy places one before a token.
leadingComment :: forall a. Comment LineFeed -> FormatDoc a -> FormatDoc a
leadingComment c next = case c of
  Comment s
    | SCU.take 2 s == "--" || SCU.take 2 s == "#!" -> TDoc.leadingLineComment s next
    | otherwise -> TDoc.leadingBlockComment s next
  Line _ n -> TDoc.sourceBreak n next
  CST.Space _ -> next

-- Markup ----------------------------------------------------------------------

-- | A laid-out piece of markup, and whether it must take several lines.
type LaidOut a = { doc :: Doc a, multiline :: Boolean }

markup :: forall a. FormatOptions Psx a -> Markup Psx -> LaidOut a
markup conf = case _ of
  MarkupElement el
    | el.name.text == "style" -> styleElement conf el
    | el.name.text == "script" -> scriptElement conf el
    | otherwise -> do
        let
          open = openTag conf el
          close = foldMap (\_ -> "</" <> el.name.text <> ">") el.close
        withChildren conf open close (if isJust el.close then el.children else [])
  MarkupFragment fr ->
    withChildren conf { doc: Dodo.text "<>", multiline: false } "</>" fr.children

-- | `<name attrs>` or `<name attrs/>`: the attributes on the tag's line, or
-- | one per line with the end on a line of its own.
openTag :: forall a. FormatOptions Psx a -> Element Psx -> LaidOut a
openTag conf el = do
  let
    attrs = map (attr conf) el.attrs
    multiline = any _.multiline attrs
    start = Dodo.text ("<" <> el.name.text)
    end = Dodo.text el.end.text
  if Array.null attrs then { doc: start <> end, multiline: false }
  else if multiline then
    { doc: start <> Dodo.indent (foldMap (\a -> Dodo.break <> a.doc) attrs) <> Dodo.break <> end, multiline }
  else
    { doc: Dodo.flexGroup (start <> Dodo.indent (foldMap (\a -> Dodo.spaceBreak <> a.doc) attrs) <> Dodo.softBreak <> end)
    , multiline
    }

attr :: forall a. FormatOptions Psx a -> Attr Psx -> LaidOut a
attr conf = case _ of
  AttrNamed a -> namedAttr conf a
  AttrTrait h -> hole conf h

namedAttr :: forall a. FormatOptions Psx a -> NamedAttr Psx -> LaidOut a
namedAttr conf a = case a.value of
  Nothing -> { doc: Dodo.text a.name.text, multiline: false }
  Just v -> do
    let
      value = case v.value of
        AttrString t -> verbatim t.text
        AttrHole h -> hole conf h
        AttrRaw t -> verbatim t.text
    value { doc = Dodo.text (a.name.text <> "=") <> value.doc }

-- | `{expr}`, or when that does not fit on the line, the expression on
-- | lines of its own between the braces.
hole :: forall a. FormatOptions Psx a -> Hole Psx -> LaidOut a
hole conf h = do
  let
    first = TokenList.head (tokensOf h.expr)
    expr = formatExpr conf h.expr
    -- Comments before the `}`.
    close = foldr leadingComment (TDoc.text "}") (Array.filter isComment h.close.leadingComments)
    -- A line comment anywhere forces line breaks, as purs-tidy finds.
    FormatDoc whole = expr `TDoc.softBreak` close
    multiline = whole.multiline || maybe false (Array.any isLineComment <<< _.leadingComments) first
    -- `{-` would open a comment, so an expression that starts with `-` or
    -- a comment starts after a space.
    open
      | maybe false (\t -> Array.any isComment t.leadingComments || startsWithMinus t.value) first = "{ "
      | otherwise = "{"
    body brk = Dodo.indent (brk <> TDoc.toDoc expr) <> brk <> TDoc.toDoc close
  if multiline then { doc: Dodo.text "{" <> body Dodo.break, multiline }
  else { doc: Dodo.flexGroup (attempt <> Dodo.text open <> body Dodo.softBreak), multiline }
  where
  isComment = case _ of
    Comment _ -> true
    _ -> false

  isLineComment = case _ of
    Comment c -> SCU.take 2 c == "--"
    _ -> false

  startsWithMinus = case _ of
    TokOperator Nothing op -> SCU.take 1 op == "-"
    _ -> false

-- | Put first in a flex group, so that the group's first try lays out the
-- | whole group on one line, groups nested in it included.
attempt :: forall a. Doc a
attempt = Dodo.flexAlt mempty mempty

-- | `<style>`: its stylesheet's lines under the element, indented one step,
-- | as they were indented relative to one another.
styleElement :: forall a. FormatOptions Psx a -> Element Psx -> LaidOut a
styleElement conf el = do
  let
    open = openTag conf el
    close = Dodo.text (foldMap (\_ -> "</style>") el.close)
    content = rawContent el
  if not (String.contains (String.Pattern "\n") content) then
    { doc: open.doc <> Dodo.text content <> close, multiline: open.multiline }
  else do
    let
      lines = map trimEnd (String.split (String.Pattern "\n") content)
      body = Array.dropWhile (_ == "") (Array.drop 1 lines)
      firstLine = trimStart (fromMaybe "" (Array.head lines))
      indentOf l = SCU.length (SCU.takeWhile (_ == ' ') l)
      common = fromMaybe 0 (Array.head (Array.sort (map indentOf (Array.filter (_ /= "") body))))
      ls = Array.cons firstLine (map (SCU.drop common) body)
        # Array.dropWhile (_ == "")
        # Array.reverse
        # Array.dropWhile (_ == "")
        # Array.reverse
    if Array.null ls then { doc: open.doc <> close, multiline: open.multiline }
    else
      { doc: open.doc <> Dodo.indent (foldMap (\l -> Dodo.break <> Dodo.text l) ls) <> Dodo.break <> close
      , multiline: true
      }

-- | `<script>`: its content exactly as written.
scriptElement :: forall a. FormatOptions Psx a -> Element Psx -> LaidOut a
scriptElement conf el = do
  let
    open = openTag conf el
    content = verbatim (rawContent el)
  { doc: open.doc <> content.doc <> Dodo.text (foldMap (\_ -> "</script>") el.close)
  , multiline: open.multiline || content.multiline
  }

rawContent :: Element Psx -> String
rawContent el = el.children # foldMap case _ of
  ChildText t -> t.text
  _ -> ""

-- | Text exactly as written: lines after the first start at column 0.
verbatim :: forall a. String -> LaidOut a
verbatim text = case Array.uncons (String.split (String.Pattern "\n") text) of
  Nothing -> { doc: mempty, multiline: false }
  Just { head, tail } ->
    { doc: Dodo.text head <> Dodo.locally (_ { indent = 0, indentSpaces = "" }) (foldMap (\l -> Dodo.break <> Dodo.text l) tail)
    , multiline: not (Array.null tail)
    }

-- | Text whose first line was at the column: its other lines keep their
-- | indentation relative to that column (less, at most, than none).
verbatimRelative :: forall a. Int -> String -> Doc a
verbatimRelative column text = case Array.uncons (map trimEnd (String.split (String.Pattern "\n") text)) of
  Nothing -> mempty
  Just { head, tail } -> Dodo.alignCurrentColumn (Dodo.text head <> foldMap (\l -> Dodo.break <> Dodo.text (dropSpaces l)) tail)
  where
  dropSpaces l = SCU.drop (min column (SCU.length (SCU.takeWhile (_ == ' ') l))) l

-- Children --------------------------------------------------------------------

-- | What children are laid out from: words of text and the other nodes, with
-- | what may separate each from the next.
data Piece a
  = Word String
  -- | `block` is an element, a fragment or a comment; a hole is not.
  | Node { laidOut :: LaidOut a, block :: Boolean }

data Sep
  -- | Nothing may come between the two: they share a line.
  = Glue
  -- | Nothing or a line break.
  | Soft
  -- | A space or a line break: between words.
  | Between

derive instance Eq Sep

-- | A child's pieces. `glueBefore` and `glueAfter` say whether whitespace
-- | the policy keeps is at its start or end; `lineBefore` and `lineAfter`
-- | whether the source has a line break there.
type Run a =
  { pieces :: Array (Piece a)
  , block :: Boolean
  , glueBefore :: Boolean
  , glueAfter :: Boolean
  , lineBefore :: Boolean
  , lineAfter :: Boolean
  }

-- | The pieces of each child. A text child makes words; its leading and
-- | trailing whitespace, where the policy keeps it, is glued to the nodes
-- | beside it, since a line break there would remove it. Text that the
-- | policy removes leaves only whether it held a line break.
runs :: forall a. FormatOptions Psx a -> Array (Child Psx) -> Array (Run a)
runs conf = Array.foldl add [] <<< map case _ of
  ChildText t -> textRun t.text
  ChildMarkup m -> Just (node true (markup conf m))
  ChildComment t -> Just (node true { doc: verbatimRelative t.range.start.column t.text, multiline: String.contains (String.Pattern "\n") t.text })
  ChildHole h -> Just (node false (hole conf h))
  ChildRaw t -> Just (node false (verbatim t.text))
  where
  node block laidOut =
    { pieces: [ Node { laidOut, block } ], block, glueBefore: false, glueAfter: false, lineBefore: false, lineAfter: false }

  -- A run with no pieces is text the policy removes.
  add acc = case _ of
    Nothing -> acc
    Just r -> case Array.unsnoc acc of
      Just { init, last }
        | Array.null last.pieces -> Array.snoc init r { lineBefore = r.lineBefore || last.lineAfter }
        | Array.null r.pieces -> Array.snoc init last { lineAfter = last.lineAfter || r.lineAfter }
      _ -> Array.snoc acc r

textRun :: forall a. String -> Maybe (Run a)
textRun raw = do
  let
    text = normalise raw
    lineBefore = String.contains (String.Pattern "\n") (SCU.takeWhile isWhitespace raw)
    lineAfter = String.contains (String.Pattern "\n") (takeWhileEnd raw)
    lead = SCU.takeWhile isWhitespace text
    trail = takeWhileEnd text
    core = SCU.drop (SCU.length lead) (SCU.dropRight (SCU.length trail) text)
    run pieces glueBefore glueAfter = Just { pieces, block: false, glueBefore, glueAfter, lineBefore, lineAfter }
  if text == "" then run [] false false
  else if core == "" then run [ Word text ] true true
  else do
    let
      words = splitWords core
      ws = Array.mapWithIndex
        ( \i w -> (if i == 0 then lead else "") <> w <> (if i == Array.length words - 1 then trail else "")
        )
        words
    run (map Word ws) (lead /= "") (trail /= "")
  where
  takeWhileEnd s = SCU.fromCharArray (Array.reverse (Array.takeWhile isWhitespace (Array.reverse (SCU.toCharArray s))))

-- | Text without whitespace at its ends, split at each single space between
-- | two characters that are not whitespace. Other runs of whitespace are
-- | kept in a word, as the policy keeps them.
splitWords :: String -> Array String
splitWords s = go [] 0 0
  where
  chars = SCU.toCharArray s
  n = Array.length chars
  at i = Array.index chars i
  ws i = maybe false isWhitespace (at i)
  go acc start i
    | i >= n = Array.snoc acc (SCU.slice start n s)
    | at i == Just ' ' && not (ws (i - 1)) && not (ws (i + 1)) = go (Array.snoc acc (SCU.slice start i s)) (i + 1) (i + 1)
    | otherwise = go acc start (i + 1)

type Laid a = { pieces :: Array (Piece a), seps :: Array Sep, lead :: Sep, trail :: Sep }

-- | The pieces in order, the separator after each but the last, and the
-- | separators after the start tag and before the end tag. Two blocks may
-- | always go on separate lines. Other nodes and text stay together unless
-- | the source has a line break between them.
lay :: forall a. Array (Run a) -> Laid a
lay rs = do
  let
    pieces = Array.concatMap _.pieces rs
    seps = Array.concat $ Array.mapWithIndex
      ( \i r -> do
          let
            inner = Array.replicate (Array.length r.pieces - 1) Between
            next = Array.index rs (i + 1) <#> \r' ->
              if r.glueAfter || r'.glueBefore then Glue
              else if (r.block && r'.block) || r.lineAfter || r'.lineBefore then Soft
              else Glue
          inner <> Array.fromFoldable next
      )
      rs
  { pieces
  , seps
  , lead: if maybe false _.glueBefore (Array.head rs) then Glue else Soft
  , trail: if maybe false _.glueAfter (Array.last rs) then Glue else Soft
  }

-- | An element or fragment with its children. `open` is the start tag and
-- | `close` the end tag, empty when there is none.
withChildren :: forall a. FormatOptions Psx a -> LaidOut a -> String -> Array (Child Psx) -> LaidOut a
withChildren conf open close children = do
  let
    laid = lay (runs conf children)
    closeDoc = Dodo.text close
    isBlock = case _ of
      Node n -> n.block
      Word _ -> false
    isMulti = case _ of
      Node n -> n.laidOut.multiline
      Word _ -> false
    broken = any isBlock laid.pieces || any isMulti laid.pieces
    -- Broken children may still share one line, when glue holds them all.
    multiline = open.multiline || any isMulti laid.pieces
      || (broken && (laid.lead == Soft || laid.trail == Soft || Array.elem Soft laid.seps))
  if Array.null laid.pieces then { doc: open.doc <> closeDoc, multiline: open.multiline }
  else if broken then
    { doc: open.doc <> Dodo.indent (edge Dodo.break laid.lead <> fill true laid) <> edge Dodo.break laid.trail <> closeDoc
    , multiline
    }
  else
    { doc: Dodo.flexGroup
        (attempt <> open.doc <> Dodo.indent (edge Dodo.softBreak laid.lead <> fill false laid) <> edge Dodo.softBreak laid.trail <> closeDoc)
    , multiline
    }
  where
  edge doc = case _ of
    Glue -> mempty
    _ -> doc

-- | The pieces, laid out when they do not fit on one line: each run of
-- | pieces glued together starts a line after a soft separator, and fills
-- | the line after a space. With `blocks`, the children never share a line
-- | with the tags, so the line breaks are unconditional.
fill :: forall a. Boolean -> Laid a -> Doc a
fill blocks laid = case Array.uncons chunks of
  Nothing -> mempty
  Just { head, tail } -> Array.foldl step (chunkDoc head) tail
  where
  chunks = Array.foldl add [] (Array.zip (Array.cons Soft laid.seps) laid.pieces)

  add acc (Tuple sep piece) = case Array.unsnoc acc of
    Just { init, last } | sep == Glue -> Array.snoc init last { pieces = Array.snoc last.pieces piece }
    _ -> Array.snoc acc { sep, pieces: [ piece ] }

  step acc chunk = case chunk.sep of
    Soft -> acc <> (if blocks then Dodo.break else Dodo.softBreak) <> chunkDoc chunk
    _ -> acc <> Dodo.flexGroup (Dodo.spaceBreak <> chunkDoc chunk)

  chunkDoc c = foldMap pieceDoc c.pieces

  pieceDoc = case _ of
    Word w -> Dodo.text w
    Node n -> n.laidOut.doc
