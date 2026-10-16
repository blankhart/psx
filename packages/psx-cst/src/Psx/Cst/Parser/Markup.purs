-- | The hook that takes the patched parser from PureScript into markup.
-- |
-- | On `<` at expression start, the hook scans markup from the raw source,
-- | parses each hole with a fresh layout stack, and resumes the module's
-- | token stream after the markup with the layout stack saved at the `<`.
-- | `markupLayout` lays out a line that starts with markup as a new
-- | statement rather than as an operator continuing the line before.
-- |
-- | The parser reads tokens, not source, so the source being parsed is held
-- | in a module-level reference that `withSource` sets around a parse.
-- | Parsing is synchronous, so nothing else can observe it.
module Psx.Cst.Parser.Markup
  ( withSource
  , currentSource
  , parseMarkup
  , recoverMarkup
  , scanAt
  , holeParser
  , isExpressionStart
  , isMarkupOperator
  , markupLayout
  , lexModuleWithMarkup
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn4, runFn2)
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Psx.Cst.Lexer (HoleParser, Scanned, isMarkupStart, scanMarkup)
import Psx.Cst.Source (Source, offsetOf, posOf)
import Psx.Cst.Source as Source
import Psx.Cst.Types (MarkupRoot, Psx(..))
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Layout (LayoutDelim(..), LayoutStack, insertLayout)
import PureScript.CST.Lexer (lexModule, lexWithState)
import PureScript.CST.Parser.Monad (Parser(..), ParserResult(..), PositionedError, initialParserState, runParser', take)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream(..), consTokens)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Expr(..), SourceToken, Token(..))

sourceRef :: Ref.Ref Source
sourceRef = unsafePerformEffect (Ref.new (Source.source ""))

-- | Runs a parse of the given source.
withSource :: forall a. Source -> (Unit -> a) -> a
withSource src k = unsafePerformEffect do
  Ref.write src sourceRef
  pure (k unit)

currentSource :: Unit -> Source
currentSource _ = unsafePerformEffect (Ref.read sourceRef)

-- | Whether the token is an operator that may open markup: one starting
-- | with `<`, which is how `<div`, `<>` and `<><li` lex.
isMarkupOperator :: SourceToken -> Boolean
isMarkupOperator tok = case tok.value of
  TokOperator Nothing op -> SCU.take 1 op == "<"
  _ -> false

-- | Whether an expression may start after this token (or at the start of a
-- | recovered region): after `=`, `(`, `[`, `,`, `->`, `<-`, `|`, `then`,
-- | `else`, `of`, `in`, a layout token, or an operator.
isExpressionStart :: Maybe SourceToken -> Boolean
isExpressionStart = case _ of
  Nothing -> true
  Just tok -> case tok.value of
    TokEquals -> true
    TokLeftParen -> true
    TokLeftSquare -> true
    TokLeftBrace -> true
    TokComma -> true
    TokRightArrow _ -> true
    TokLeftArrow _ -> true
    TokPipe -> true
    TokBackslash -> false
    TokOperator _ _ -> true
    TokLayoutStart _ -> true
    TokLayoutSep _ -> true
    TokLowerName Nothing kw -> kw == "then" || kw == "else" || kw == "of" || kw == "in" || kw == "if" || kw == "case"
    _ -> false

-- | Parses the hole whose `{` is at the offset, from just after the `{`
-- | with a fresh layout stack holding only a brace delimiter. (Lexing from
-- | the `{` itself would read the first word as a record label.)
holeParser :: Parser (Expr Psx) -> Source -> HoleParser Psx
holeParser pExpr src i = do
  let
    pos = posOf src i
    stack = Tuple pos LytBrace : Tuple pos LytRoot : Nil
    stream = markupLayout src stack (lexWithState stack (posOf src (i + 1)) (SCU.drop (i + 1) src.text))
    p = Tuple <$> pExpr <*> take \tok -> case tok.value of
      TokRightBrace -> Right tok
      other -> Left (UnexpectedToken other)
  case runParser' (initialParserState stream) p of
    ParseSucc (Tuple expr close) state ->
      -- The lexer gives `}` the comments after it on its line, which here
      -- are markup text.
      Right { expr, close: close { trailingComments = [] }, end: offsetOf src close.range.end, errors: state.errors }
    ParseFail err _ -> Left err

-- | The markup at a `<` token, if markup starts there, and the module's
-- | token stream resumed after it.
scanAt :: Parser (Expr Psx) -> SourceToken -> LayoutStack -> Maybe { root :: MarkupRoot, stream :: TokenStream, holeErrors :: Array PositionedError }
scanAt pExpr tok stk = do
  let src = currentSource unit
  scanned :: Scanned Psx <- scanMarkup (holeParser pExpr src) src (offsetOf src tok.range.start)
  pure
    { root: { leadingComments: tok.leadingComments, markup: scanned.markup, errors: scanned.errors }
    , stream: markupLayout src stk (lexWithState stk (posOf src scanned.end) (SCU.drop scanned.end src.text))
    , holeErrors: scanned.holeErrors
    }

-- | Markup at expression start.
parseMarkup :: Parser (Expr Psx) -> Parser (Expr Psx)
parseMarkup pExpr = Parser
  ( mkFn4 \state _ resume done ->
      case TokenStream.step state.stream of
        TokenCons tok _ _ stk
          | isMarkupOperator tok
          , Just r <- scanAt pExpr tok stk ->
              runFn2 done
                (state { stream = r.stream, consumed = true, errors = state.errors <> r.holeErrors })
                (ExprError (PsxMarkup r.root))
          | otherwise ->
              runFn2 resume state { position: tok.range.start, error: UnexpectedToken tok.value }
        TokenEOF pos _ ->
          runFn2 resume state { position: pos, error: UnexpectedEof }
        TokenError pos error _ _ ->
          runFn2 resume state { position: pos, error }
  )

-- | Markup at a token during error recovery.
recoverMarkup :: Parser (Expr Psx) -> SourceToken -> LayoutStack -> Maybe (Tuple MarkupRoot TokenStream)
recoverMarkup pExpr tok stk
  | isMarkupOperator tok = scanAt pExpr tok stk <#> \r -> Tuple r.root r.stream
  | otherwise = Nothing

-- Layout ----------------------------------------------------------------------

-- | The module's token stream, with markup laid out as an expression.
lexModuleWithMarkup :: Source -> TokenStream
lexModuleWithMarkup src = markupLayout src (Tuple { line: 0, column: 0 } LytRoot : Nil) (lexModule src.text)

-- | cst-parser lays out a line that starts with an operator as the
-- | continuation of the line before: at a block's column it closes the block.
-- | Markup at the start of a line is an expression, so a do statement can be
-- | markup. Where markup starts at an operator and the layout differs, this
-- | lays the token out as an ordinary expression token instead, and re-lexes
-- | what follows with the resulting stack. `stack` is the layout stack before
-- | the stream's first token.
markupLayout :: Source -> LayoutStack -> TokenStream -> TokenStream
markupLayout src stack stream = TokenStream $ Lazy.defer \_ ->
  case TokenStream.step stream of
    step@(TokenCons tok _ next stk) -> case markupAfterLayout stream of
      Just { real, generated }
        | Just toks <- relaid real generated ->
            TokenStream.step (snd (consTokens toks.tokens (Tuple real.range.end (relex toks.stack real))))
      _ -> case step of
        TokenCons _ pos _ _ -> TokenCons tok pos (markupLayout src stk next) stk
        other -> other
    other -> other
  where
  -- The layout cst-parser would give the token as an expression token, if
  -- that differs from what it gave it as an operator.
  relaid real generated = do
    let
      Tuple stack' toks = insertLayout (real { value = TokString "" "" }) real.range.end stack
      tokens = toks <#> \(Tuple t s) -> if t.range == real.range then Tuple real s else Tuple t s
    if map (_.value <<< fst) tokens == map _.value (Array.snoc generated real) then Nothing
    else Just { stack: stack', tokens }

  relex stack' real = do
    let offset = offsetOf src real.range.end
    markupLayout src stack' (lexWithState stack' real.range.end (SCU.drop offset src.text))

  -- If the stream starts with the layout tokens cst-parser generated for a
  -- `<` operator that starts markup, then that operator.
  markupAfterLayout = go []
    where
    go generated s = case TokenStream.step s of
      TokenCons t _ next _
        | isGeneratedEnd t -> go (Array.snoc generated t) next
        | isMarkupOperator t
        , isMarkupStart src (offsetOf src t.range.start)
        , Array.all (\g -> g.range.start == t.range.start) generated -> Just { real: t, generated }
      _ -> Nothing

  isGeneratedEnd t = case t.value of
    TokLayoutEnd _ -> true
    TokLayoutSep _ -> true
    _ -> false
