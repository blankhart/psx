-- | The markup scanner. It reads markup from raw source at a `<`, and hands
-- | each hole to a callback that parses PureScript from just after its `{`.
-- |
-- | The scanner never fails once it has seen `<` followed by a name or `>`:
-- | errors are recorded and the source they cover is kept as raw nodes, so
-- | the tree still prints back exactly.
module Psx.Cst.Lexer
  ( HoleParser
  , HoleResult
  , Scanned
  , scanMarkup
  , isMarkupStart
  ) where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Psx.Cst.Source (Source, charAt, posOf, slice, startsWith)
import Psx.Cst.Types (Attr, AttrValue(..), Child(..), Element, ElementClose, Fragment, Hole, MToken, Markup(..), PsxError)
import Psx.HtmlData as HtmlData
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (Expr, SourceToken)

-- | Parses the hole whose `{` is at the offset. On success, `end` is the
-- | offset just after its `}` and `errors` are cst-parser's recovered ones.
type HoleParser e = Int -> Either PositionedError (HoleResult e)

type HoleResult e =
  { expr :: Expr e
  , close :: SourceToken
  , end :: Int
  , errors :: Array PositionedError
  }

type Scanned e =
  { markup :: Markup e
  , end :: Int
  , errors :: Array PsxError
  -- | cst-parser's recovered errors from inside holes
  , holeErrors :: Array PositionedError
  }

data Namespace = Html | Svg

derive instance Eq Namespace

type Ctx e =
  { src :: Source
  , hole :: HoleParser e
  , ns :: Namespace
  -- | Open elements, innermost first; a fragment is "".
  , open :: List String
  }

-- | Errors and hole errors gathered while scanning.
-- | `unclosed` records that an element ran to the end of the input.
type Acc = { errors :: Array PsxError, holeErrors :: Array PositionedError, unclosed :: Boolean }

noAcc :: Acc
noAcc = { errors: [], holeErrors: [], unclosed: false }

addError :: PsxError -> Acc -> Acc
addError e acc = acc { errors = Array.snoc acc.errors e }

appendAcc :: Acc -> Acc -> Acc
appendAcc a b = { errors: a.errors <> b.errors, holeErrors: a.holeErrors <> b.holeErrors, unclosed: a.unclosed || b.unclosed }

-- | Whether markup starts at the offset: `<` followed by a letter or `>`.
isMarkupStart :: Source -> Int -> Boolean
isMarkupStart src i = charAt src i == Just '<' && case charAt src (i + 1) of
  Just '>' -> true
  Just c -> isAsciiLetter c
  Nothing -> false

-- | Scans the markup at the offset, or nothing if markup doesn't start there.
-- |
-- | An element left open runs to the end of the input. So that it doesn't
-- | swallow the rest of the module, markup with an unclosed element is
-- | scanned again, ending before the first later line indented no further
-- | than the line the markup starts on.
scanMarkup :: forall e. HoleParser e -> Source -> Int -> Maybe (Scanned e)
scanMarkup holeP src i
  | isMarkupStart src i = do
      let
        scan s = markup { src: s, hole: holeP, ns: Html, open: Nil } noAcc i
        r = case scan src of
          r1 | r1.acc.unclosed, Just limit <- outdentedLine src i -> scan (src { text = SCU.take limit src.text })
          r1 -> r1
      Just { markup: r.value, end: r.end, errors: r.acc.errors, holeErrors: r.acc.holeErrors }
  | otherwise = Nothing

-- | The offset of the first line after the one holding the offset that is
-- | not blank and is indented no further than that line.
outdentedLine :: Source -> Int -> Maybe Int
outdentedLine src i = do
  let
    line = (posOf src i).line
    indentOf start = spanWhile (\c -> c == ' ' || c == '\t') src start - start
    indent = maybe 0 indentOf (Array.index src.lineStarts line)
    blank start = case charAt src (start + indentOf start) of
      Nothing -> true
      Just c -> c == '\n' || c == '\r'
  Array.find (\start -> not (blank start) && indentOf start <= indent) (Array.drop (line + 1) src.lineStarts)

type Step a = { value :: a, end :: Int, acc :: Acc }

markup :: forall e. Ctx e -> Acc -> Int -> Step (Markup e)
markup ctx acc i
  | charAt ctx.src (i + 1) == Just '>' = do
      let r = fragment ctx acc i
      r { value = MarkupFragment r.value }
  | otherwise = do
      let r = element ctx acc i
      r { value = MarkupElement r.value }

-- Tokens ----------------------------------------------------------------------

token :: Source -> String -> Int -> Int -> MToken
token src leading from to =
  { leading
  , text: slice src from to
  , range: { start: posOf src from, end: posOf src to }
  }

emptyToken :: Source -> String -> Int -> MToken
emptyToken src leading i = token src leading i i

errorAt :: Source -> Int -> Int -> String -> PsxError
errorAt src from to message =
  { range: { start: posOf src from, end: posOf src to }, message, fix: Nothing }

-- | The end of the run of characters satisfying the predicate.
spanWhile :: (Char -> Boolean) -> Source -> Int -> Int
spanWhile p src i = case charAt src i of
  Just c | p c -> spanWhile p src (i + 1)
  _ -> i

-- | The offset of the next occurrence of the string, or the end of the source.
findFrom :: Source -> String -> Int -> Maybe Int
findFrom src s i = (_ + i) <$> SCU.indexOf (String.Pattern s) (SCU.drop i src.text)

isAsciiLetter :: Char -> Boolean
isAsciiLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t' || toCharCode c == 12

isTagNameChar :: Char -> Boolean
isTagNameChar c = isAsciiLetter c || (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == ':'

isAttrNameChar :: Char -> Boolean
isAttrNameChar c = not (isSpace c) && not (Array.elem c [ '"', '\'', '<', '>', '/', '=', '{', '}', '`' ])

-- Elements --------------------------------------------------------------------

element :: forall e. Ctx e -> Acc -> Int -> Step (Element e)
element ctx acc0 i = do
  let
    src = ctx.src
    open = token src "" i (i + 1)
    nameEnd = spanWhile isTagNameChar src (i + 1)
    name = token src "" (i + 1) nameEnd
    acc1 =
      if Just true == (isUpper <$> charAt src (i + 1)) then
        addError
          ( errorAt src i nameEnd
              ( "<" <> name.text <> "> is a capitalised tag. PSX has no component tags: call the function in a hole, as in {"
                  <> lowerFirst name.text
                  <> " ...}"
              )
          )
          acc0
      else acc0
    tag = attrs ctx acc1 nameEnd []
    childNs
      | ctx.ns == Html && name.text == "svg" = Svg
      | ctx.ns == Svg && name.text == "foreignObject" = Html
      | otherwise = ctx.ns
    noChildren a end = { value: { open, name, attrs: tag.attrs, end: tag.endToken, children: [], close: Nothing }, end, acc: a }
  case tag.ending of
    SelfClosed -> noChildren tag.acc tag.end
    Unterminated -> noChildren tag.acc tag.end
    Opened
      | ctx.ns == Html && HtmlData.isVoid name.text -> noChildren tag.acc tag.end
      | name.text == "style" || name.text == "script" -> do
          let
            contentEnd = case findFrom src ("</" <> name.text) tag.end of
              Just j -> j
              Nothing -> SCU.length src.text
            content = if contentEnd > tag.end then [ ChildText (token src "" tag.end contentEnd) ] else []
          if contentEnd >= SCU.length src.text then
            { value: { open, name, attrs: tag.attrs, end: tag.endToken, children: content, close: Nothing }
            , end: contentEnd
            , acc: (addError (errorAt src i tag.end ("<" <> name.text <> "> is not closed")) tag.acc) { unclosed = true }
            }
          else do
            let c = closeTag src contentEnd
            { value: { open, name, attrs: tag.attrs, end: tag.endToken, children: content, close: Just c.value }
            , end: c.end
            , acc: tag.acc
            }
      | otherwise -> do
          let ctx' = ctx { ns = childNs, open = name.text : ctx.open }
          case children ctx' noAcc tag.end [] of
            Closed r ->
              { value: { open, name, attrs: tag.attrs, end: tag.endToken, children: r.children, close: Just r.close }
              , end: r.end
              , acc: appendAcc tag.acc r.acc
              }
            ImplicitlyClosed r ->
              { value: { open, name, attrs: tag.attrs, end: tag.endToken, children: r.children, close: Nothing }
              , end: r.end
              , acc: addError (errorAt src i tag.end ("<" <> name.text <> "> is not closed before </" <> r.by <> ">")) (appendAcc tag.acc r.acc)
              }
            EndOfFile r ->
              { value: { open, name, attrs: tag.attrs, end: tag.endToken, children: r.children, close: Nothing }
              , end: r.end
              , acc: (addError (errorAt src i tag.end ("<" <> name.text <> "> is not closed")) (appendAcc tag.acc r.acc)) { unclosed = true }
              }

lowerFirst :: String -> String
lowerFirst s = String.toLower (SCU.take 1 s) <> SCU.drop 1 s

data TagEnding = Opened | SelfClosed | Unterminated

type Tag e =
  { attrs :: Array (Attr e)
  , endToken :: MToken
  , ending :: TagEnding
  , end :: Int
  , acc :: Acc
  }

attrs :: forall e. Ctx e -> Acc -> Int -> Array (Attr e) -> Tag e
attrs ctx acc i done = do
  let
    src = ctx.src
    j = spanWhile isSpace src i
    ws = slice src i j
  case charAt src j of
    Nothing ->
      { attrs: done, endToken: emptyToken src ws j, ending: Unterminated, end: j, acc: addError (errorAt src j j "Expected > or /> to end the tag") acc }
    Just '>' ->
      { attrs: done, endToken: token src ws j (j + 1), ending: Opened, end: j + 1, acc }
    Just '/'
      | startsWith src j "/>" ->
          { attrs: done, endToken: token src ws j (j + 2), ending: SelfClosed, end: j + 2, acc }
    Just '<' ->
      { attrs: done, endToken: emptyToken src ws j, ending: Unterminated, end: j, acc: addError (errorAt src j j "Expected > or /> to end the tag") acc }
    Just c
      | isAttrNameChar c -> do
          let
            nameEnd = spanWhile isAttrNameChar src j
            name = token src ws j nameEnd
            k = spanWhile isSpace src nameEnd
          if charAt src k == Just '=' then do
            let
              equals = token src (slice src nameEnd k) k (k + 1)
              v = attrValue ctx acc (k + 1)
            attrs ctx v.acc v.end (Array.snoc done { name, value: Just { equals, value: v.value } })
          else attrs ctx acc nameEnd (Array.snoc done { name, value: Nothing })
      | otherwise -> do
          -- A stray character: keep it as an attribute name, with an error.
          let name = token src ws j (j + 1)
          attrs ctx (addError (errorAt src j (j + 1) ("Unexpected " <> SCU.singleton c <> " in a tag")) acc) (j + 1) (Array.snoc done { name, value: Nothing })

attrValue :: forall e. Ctx e -> Acc -> Int -> Step (AttrValue e)
attrValue ctx acc i = do
  let
    src = ctx.src
    j = spanWhile isSpace src i
    ws = slice src i j
    quoted q = case findFrom src q (j + 1) of
      Just k -> { value: AttrString (token src ws j (k + 1)), end: k + 1, acc }
      Nothing -> do
        let k = lineEnd src j
        { value: AttrRaw (token src ws j k), end: k, acc: addError (errorAt src j k "Unterminated attribute value") acc }
  case charAt src j of
    Just '"' -> quoted "\""
    Just '\'' -> quoted "'"
    Just '{' -> do
      let h = hole ctx acc ws j
      h { value = either AttrRaw AttrHole h.value }
    _ ->
      { value: AttrRaw (emptyToken src ws j)
      , end: j
      , acc: addError (errorAt src j j "Expected a quoted value or a hole {expr} after =") acc
      }
  where
  either f g = case _ of
    Left a -> f a
    Right b -> g b

lineEnd :: Source -> Int -> Int
lineEnd src i = spanWhile (\c -> c /= '\n' && c /= '\r') src i

-- | A hole at `{`. On a PureScript error, the hole up to its matching `}` is
-- | kept raw.
hole :: forall e. Ctx e -> Acc -> String -> Int -> Step (Either MToken (Hole e))
hole ctx acc ws i = case ctx.hole i of
  Right r ->
    { value: Right { open: token ctx.src ws i (i + 1), expr: r.expr, close: r.close }
    , end: r.end
    , acc: acc { holeErrors = acc.holeErrors <> r.errors }
    }
  Left err -> do
    let
      end = case matchingBrace ctx.src (i + 1) of
        Just k -> k + 1
        Nothing -> lineEnd ctx.src i
      pos = err.position
    { value: Left (token ctx.src ws i end)
    , end
    , acc: addError { range: { start: pos, end: pos }, message: printError err, fix: Nothing } acc
    }
  where
  printError err = "In a hole: " <> printParseError err.error

-- | The offset of the `}` that closes a hole whose contents start at the
-- | offset, skipping strings, comments and nested braces. Used to recover from
-- | a hole that doesn't parse.
matchingBrace :: Source -> Int -> Maybe Int
matchingBrace src = go 0
  where
  go depth i = case charAt src i of
    Nothing -> Nothing
    Just '}'
      | depth == 0 -> Just i
      | otherwise -> go (depth - 1) (i + 1)
    Just '{'
      | startsWith src i "{-" -> skipTo "-}" (i + 2) >>= go depth
      | otherwise -> go (depth + 1) (i + 1)
    Just '-'
      | startsWith src i "--" -> go depth (lineEnd src i)
    Just '"'
      | startsWith src i "\"\"\"" -> skipTo "\"\"\"" (i + 3) >>= go depth
      | otherwise -> stringEnd (i + 1) >>= go depth
    _ -> go depth (i + 1)

  skipTo s i = (_ + SCU.length s) <$> findFrom src s i

  stringEnd i = case charAt src i of
    Nothing -> Nothing
    Just '\\' -> stringEnd (i + 2)
    Just '"' -> Just (i + 1)
    Just '\n' -> Just i
    _ -> stringEnd (i + 1)

-- | `</name>` (or `</>`) at the offset.
closeTag :: Source -> Int -> Step ElementClose
closeTag src i = do
  let
    nameEnd = spanWhile isTagNameChar src (i + 2)
    j = spanWhile isSpace src nameEnd
    open = token src "" i (i + 2)
    name = token src "" (i + 2) nameEnd
  if charAt src j == Just '>' then
    { value: { open, name, end: Just (token src (slice src nameEnd j) j (j + 1)) }, end: j + 1, acc: noAcc }
  else
    { value: { open, name, end: Nothing }, end: nameEnd, acc: addError (errorAt src i nameEnd "Expected > to end the closing tag") noAcc }

-- Fragments -------------------------------------------------------------------

fragment :: forall e. Ctx e -> Acc -> Int -> Step (Fragment e)
fragment ctx acc i = do
  let
    src = ctx.src
    open = token src "" i (i + 2)
  case children (ctx { open = "" : ctx.open }) noAcc (i + 2) [] of
    Closed r ->
      { value: { open, children: r.children, close: Just (closeToken r.close) }, end: r.end, acc: appendAcc acc r.acc }
    ImplicitlyClosed r ->
      { value: { open, children: r.children, close: Nothing }
      , end: r.end
      , acc: addError (errorAt src i (i + 2) ("<> is not closed before </" <> r.by <> ">")) (appendAcc acc r.acc)
      }
    EndOfFile r ->
      { value: { open, children: r.children, close: Nothing }
      , end: r.end
      , acc: (addError (errorAt src i (i + 2) "<> is not closed") (appendAcc acc r.acc)) { unclosed = true }
      }
  where
  -- `</>` as one token, with any whitespace inside it.
  closeToken c = do
    let endTok = case c.end of
          Just t -> t
          Nothing -> c.name
    { leading: "", text: c.open.text <> c.name.leading <> c.name.text <> endTok.leading <> endTok.text, range: { start: c.open.range.start, end: endTok.range.end } }

-- Children --------------------------------------------------------------------

data ChildrenEnd e
  = Closed { children :: Array (Child e), close :: ElementClose, end :: Int, acc :: Acc }
  -- | An ancestor's closing tag, which is left for the ancestor.
  | ImplicitlyClosed { children :: Array (Child e), by :: String, end :: Int, acc :: Acc }
  | EndOfFile { children :: Array (Child e), end :: Int, acc :: Acc }

children :: forall e. Ctx e -> Acc -> Int -> Array (Child e) -> ChildrenEnd e
children ctx acc i done = do
  let src = ctx.src
  case charAt src i of
    Nothing -> EndOfFile { children: done, end: i, acc }
    Just '<'
      | startsWith src i "</" -> do
          let
            c = closeTag src i
            closing = c.value.name.text
          case ctx.open of
            current : _ | current == closing ->
              Closed { children: done, close: c.value, end: c.end, acc: appendAcc acc c.acc }
            _ : ancestors | List.elem closing ancestors ->
              ImplicitlyClosed { children: done, by: closing, end: i, acc }
            _ -> do
              let
                what = if closing == "" then "</>" else "</" <> closing <> ">"
                err = errorAt src i c.end ("Unexpected " <> what <> ": no element is open with that name")
              children ctx (addError err acc) c.end (Array.snoc done (ChildRaw (token src "" i c.end)))
      | startsWith src i "<!--" -> do
          let
            end = case findFrom src "-->" (i + 4) of
              Just k -> Just (k + 3)
              Nothing -> Nothing
          case end of
            Just k -> children ctx acc k (Array.snoc done (ChildComment (token src "" i k)))
            Nothing -> do
              let k = SCU.length src.text
              children ctx (addError (errorAt src i (i + 4) "Unterminated comment: expected -->") acc) k
                (Array.snoc done (ChildRaw (token src "" i k)))
      | isMarkupStart src i -> do
          let r = markup ctx acc i
          children ctx r.acc r.end (Array.snoc done (ChildMarkup r.value))
      | otherwise ->
          children ctx (addError (errorAt src i (i + 1) "A < in text must be written &lt;") acc) (i + 1)
            (Array.snoc done (ChildRaw (token src "" i (i + 1))))
    Just '{' -> do
      let h = hole ctx acc "" i
      children ctx h.acc h.end (Array.snoc done (either ChildRaw ChildHole h.value))
    Just '}' ->
      children ctx (addError (errorAt src i (i + 1) "A } in text must be written {\"}\"}") acc) (i + 1)
        (Array.snoc done (ChildRaw (token src "" i (i + 1))))
    Just _ -> do
      let end = spanWhile (\c -> c /= '<' && c /= '{' && c /= '}') src i
      children ctx acc end (Array.snoc done (ChildText (token src "" i end)))
  where
  either f g = case _ of
    Left a -> f a
    Right b -> g b
