-- | Parses `.psx` modules: PureScript in which a markup literal may stand
-- | wherever an expression starts.
module Psx.Cst.Parser
  ( ParseResult
  , parseModule
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.String.CodeUnits as SCU
import Psx.Cst.Lexer (scanMarkup)
import Psx.Cst.Parser.Markup (holeParser, isExpressionStart, isMarkupOperator, lexModuleWithMarkup, withSource)
import Psx.Cst.Parser.PureScript as Purs
import Psx.Cst.Source (Source, offsetOf, posOf, source)
import Psx.Cst.Traversal (markupErrors)
import Psx.Cst.Types (Psx, PsxError)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (ParserResult(..), PositionedError, initialParserState, runParser')
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Module, SourcePos, SourceRange, SourceToken)

-- | The module, unless the parse failed outright, and every error in source
-- | order. With errors, the module holds recovered nodes; it still prints
-- | back to the source exactly.
type ParseResult =
  { module :: Maybe (Module Psx)
  , errors :: Array PsxError
  }

parseModule :: String -> ParseResult
parseModule text = do
  let result = parseOnce (source text)
  if Array.null result.errors then result
  else result { errors = diagnoseArguments (source text) result.errors }

parseOnce :: Source -> ParseResult
parseOnce src = withSource src \_ ->
  case runParser' (initialParserState (lexModuleWithMarkup src)) Purs.parseModule of
    ParseSucc m state ->
      { module: Just m, errors: sortErrors (map fromPositioned state.errors <> markupErrors m) }
    ParseFail err _ ->
      { module: Nothing, errors: [ fromPositioned err ] }

fromPositioned :: PositionedError -> PsxError
fromPositioned { position, error } =
  { range: { start: position, end: position }, message: printParseError error, fix: Nothing }

sortErrors :: Array PsxError -> Array PsxError
sortErrors = Array.sortWith \e -> [ e.range.start.line, e.range.start.column ]

-- Markup as an argument -------------------------------------------------------

-- `f <div/>` lexes as `f < div />`, so it fails as PureScript somewhere
-- after the `<`. When it does, each `<` that follows the end of an
-- expression and starts well-formed markup is tried in parentheses. A
-- candidate is kept if the module then parses with fewer errors, or with
-- its first error later, and the search repeats on the fixed source. Each
-- kept candidate is reported with its fix; the other errors are those of
-- the fixed source, mapped back.

type Progress = { failed :: Boolean, count :: Int, first :: Int }

-- | The text being tried and the offsets of the parentheses inserted into
-- | it, ascending.
type Attempt = { src :: Source, inserted :: Array Int }

diagnoseArguments :: Source -> Array PsxError -> Array PsxError
diagnoseArguments original errors0 = go 0 { src: original, inserted: [] } [] (progress original { src: original, inserted: [] } (parseOnce original))
  where
  go :: Int -> Attempt -> Array PsxError -> Progress -> Array PsxError
  go n attempt found prog = case if n >= 50 then Nothing else improve attempt prog (candidates attempt.src) of
    Just r -> go (n + 1) r.attempt (Array.snoc found r.error) r.progress
    Nothing
      | Array.null found -> errors0
      | otherwise -> sortErrors (found <> map (mapError attempt) (parseOnce attempt.src).errors)

  improve attempt prog cands = case Array.uncons cands of
    Nothing -> Nothing
    Just { head: c, tail } -> do
      let
        fixed = insertParens attempt c
        result = parseOnce fixed.src
        prog' = progress original fixed result
      if better prog' prog then
        Just { attempt: fixed, progress: prog', error: argumentError attempt c }
      else improve attempt prog tail

  argumentError attempt c = do
    let
      start = posOf original (toOriginal attempt c.start)
      end = posOf original (toOriginal attempt c.end)
    { range: { start, end }
    , message: "Markup as a function argument must be parenthesised"
    , fix: Just
        { message: "Add parentheses"
        , edits: [ { range: { start, end: start }, text: "(" }, { range: { start: end, end }, text: ")" } ]
        }
    }

  mapError attempt e = e { range = mapRange attempt e.range }

  mapRange :: Attempt -> SourceRange -> SourceRange
  mapRange attempt r = { start: mapPos attempt r.start, end: mapPos attempt r.end }

  mapPos :: Attempt -> SourcePos -> SourcePos
  mapPos attempt pos = posOf original (toOriginal attempt (offsetOf attempt.src pos))

better :: Progress -> Progress -> Boolean
better a b
  | a.failed /= b.failed = not a.failed
  | a.count /= b.count = a.count < b.count
  | otherwise = a.first > b.first

progress :: Source -> Attempt -> ParseResult -> Progress
progress original attempt result =
  { failed: isNothing result.module
  , count: Array.length result.errors
  , first: case Array.head result.errors of
      Just e -> toOriginal attempt (offsetOf attempt.src e.range.start)
      Nothing -> SCU.length original.text
  }

toOriginal :: Attempt -> Int -> Int
toOriginal attempt offset = offset - Array.length (Array.filter (_ < offset) attempt.inserted)

insertParens :: Attempt -> { start :: Int, end :: Int } -> Attempt
insertParens attempt c = do
  let
    text = attempt.src.text
    text' = SCU.take c.start text <> "(" <> SCU.slice c.start c.end text <> ")" <> SCU.drop c.end text
    shift o
      | o >= c.end = o + 2
      | o >= c.start = o + 1
      | otherwise = o
  { src: source text'
  , inserted: Array.sort (map shift attempt.inserted <> [ c.start, c.end + 1 ])
  }

-- | Each `<` after the end of an expression at which well-formed markup
-- | starts, with the offsets the markup spans.
candidates :: Source -> Array { start :: Int, end :: Int }
candidates src = withSource src \_ -> go [] Nothing (lexModuleWithMarkup src)
  where
  go :: Array { start :: Int, end :: Int } -> Maybe SourceToken -> TokenStream -> Array { start :: Int, end :: Int }
  go acc prev stream = case TokenStream.step stream of
    TokenCons tok _ next _
      | isMarkupOperator tok && not (isExpressionStart prev) ->
          go (acc <> candidate (offsetOf src tok.range.start)) (Just tok) next
      | otherwise -> go acc (Just tok) next
    _ -> acc

  candidate offset = case scanMarkup (holeParser Purs.parseExpr src) src offset of
    Just s | Array.null s.errors && Array.null s.holeErrors -> [ { start: offset, end: s.end } ]
    _ -> []

