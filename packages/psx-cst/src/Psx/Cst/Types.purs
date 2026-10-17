-- | The concrete syntax of markup, carried inside cst-parser's tree.
-- |
-- | cst-parser's types are parameterised by the payload of their error
-- | nodes. A `.psx` module is a `Module Psx`: a markup literal is the
-- | expression `ExprError (PsxMarkup root)`, and cst-parser's own recovered
-- | errors are `PsxRecovered`. Holes hold `Expr Psx` again, so markup nests
-- | to any depth.
-- |
-- | Every piece of markup syntax is kept with the whitespace before it, so
-- | the tree prints back to its source exactly.
module Psx.Cst.Types
  ( Psx(..)
  , MarkupRoot
  , Markup(..)
  , Element
  , ElementClose
  , Fragment
  , Child(..)
  , Attr(..)
  , NamedAttr
  , namedAttrs
  , AttrValue(..)
  , Hole
  , MToken
  , PsxError
  , Fix
  , Edit
  , markupTokens
  , mtokenSource
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import PureScript.CST.Errors (RecoveredError)
import PureScript.CST.Range (class RangeOf, class TokensOf, rangeOf, tokensOf)
import PureScript.CST.Range.TokenList (TokenList)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Comment(..), Expr, LineFeed(..), SourceRange, SourceToken, Token(..))

data Psx
  = PsxMarkup MarkupRoot
  | PsxRecovered RecoveredError

-- | A markup literal at expression start. `leadingComments` are the
-- | whitespace and PureScript comments before its `<`. `errors` are the ones
-- | found scanning it, not those of markup nested in its holes.
type MarkupRoot =
  { leadingComments :: Array (Comment LineFeed)
  , markup :: Markup Psx
  , errors :: Array PsxError
  }

data Markup e
  = MarkupElement (Element e)
  | MarkupFragment (Fragment e)

-- | `<name attrs>children</name>`, `<name attrs/>`, or a void element
-- | `<name attrs>`. `end` is `>` or `/>`; `close` is absent for `/>`, void
-- | elements and unclosed elements.
type Element e =
  { open :: MToken
  , name :: MToken
  , attrs :: Array (Attr e)
  , end :: MToken
  , children :: Array (Child e)
  , close :: Maybe ElementClose
  }

-- | `</`, the name and `>`. A close tag with a missing `>` has none.
type ElementClose =
  { open :: MToken
  , name :: MToken
  , end :: Maybe MToken
  }

-- | `<>children</>`. `close` is absent when the fragment is unclosed.
type Fragment e =
  { open :: MToken
  , children :: Array (Child e)
  , close :: Maybe MToken
  }

data Child e
  -- | Text exactly as written: whitespace and entities are left for the
  -- | lowering.
  = ChildText MToken
  | ChildMarkup (Markup e)
  -- | `<!-- ... -->`
  | ChildComment MToken
  | ChildHole (Hole e)
  -- | Source skipped after an error, kept so the tree still prints back.
  | ChildRaw MToken

-- | What stands among an element's attributes: a named attribute, or a
-- | trait hole `{expr}`, which holds one of the backend's `Attr`s.
data Attr e
  = AttrNamed (NamedAttr e)
  | AttrTrait (Hole e)

-- | The named attributes among an element's, in order.
namedAttrs :: forall e. Array (Attr e) -> Array (NamedAttr e)
namedAttrs = Array.mapMaybe case _ of
  AttrNamed a -> Just a
  AttrTrait _ -> Nothing

-- | `name`, `name="text"`, `name='text'` or `name={expr}`.
type NamedAttr e =
  { name :: MToken
  , value :: Maybe { equals :: MToken, value :: AttrValue e }
  }

data AttrValue e
  -- | The quoted value, quotes included.
  = AttrString MToken
  | AttrHole (Hole e)
  -- | Source skipped after an error.
  | AttrRaw MToken

-- | `{expr}`. `close` is the `}` as cst-parser lexed it, with the whitespace
-- | and comments before it.
type Hole e =
  { open :: MToken
  , expr :: Expr e
  , close :: SourceToken
  }

-- | A piece of markup syntax: its text, the whitespace before it (inside a
-- | tag), and the range of the text.
type MToken =
  { leading :: String
  , text :: String
  , range :: SourceRange
  }

-- | An error with the range it covers and, where one is known, a fix.
type PsxError =
  { range :: SourceRange
  , message :: String
  , fix :: Maybe Fix
  }

type Fix =
  { message :: String
  , edits :: Array Edit
  }

-- | Replace the source in `range` with `text`; an empty range inserts.
type Edit =
  { range :: SourceRange
  , text :: String
  }

mtokenSource :: MToken -> String
mtokenSource t = t.leading <> t.text

-- Markup travels through cst-parser's token lists as opaque tokens: an
-- operator token, with no module, whose name is the source text. cst-parser
-- prints such a token verbatim, so `printModule` prints a PSX module
-- exactly. Whitespace inside tags becomes the token's leading comments.
opaque :: MToken -> SourceToken
opaque t =
  { range: t.range
  , leadingComments: whitespaceComments t.leading
  , trailingComments: []
  , value: TokOperator Nothing t.text
  }

whitespaceComments :: String -> Array (Comment LineFeed)
whitespaceComments = go []
  where
  go acc s
    | s == "" = acc
    | SCU.take 2 s == "\r\n" = go (Array.snoc acc (Line CRLF 1)) (SCU.drop 2 s)
    | SCU.take 1 s == "\n" = go (Array.snoc acc (Line LF 1)) (SCU.drop 1 s)
    | SCU.take 1 s == " " = do
        let n = SCU.length (String.takeWhile (eq (String.codePointFromChar ' ')) s)
        go (Array.snoc acc (Space n)) (SCU.drop n s)
    | otherwise = do
        let
          n = SCU.length
            ( String.takeWhile
                (\c -> c /= String.codePointFromChar ' ' && c /= String.codePointFromChar '\n' && c /= String.codePointFromChar '\r')
                s
            )
          n' = if n == 0 then 1 else n
        go (Array.snoc acc (Comment (SCU.take n' s))) (SCU.drop n' s)

-- | The tokens of a markup tree, with the hole expressions' own tokens.
markupTokens :: Markup Psx -> TokenList
markupTokens = case _ of
  MarkupElement el ->
    TokenList.cons (opaque el.open)
      $ TokenList.cons (opaque el.name)
      $ foldMap attrTokens el.attrs
      <> TokenList.singleton (opaque el.end)
      <> foldMap childTokens el.children
      <> foldMap closeTokens el.close
  MarkupFragment fr ->
    TokenList.cons (opaque fr.open)
      $ foldMap childTokens fr.children
      <> foldMap (TokenList.singleton <<< opaque) fr.close
  where
  closeTokens c =
    TokenList.cons (opaque c.open)
      $ TokenList.cons (opaque c.name)
      $ foldMap (TokenList.singleton <<< opaque) c.end

  attrTokens = case _ of
    AttrNamed a -> namedTokens a
    AttrTrait h -> holeTokens h

  namedTokens a =
    TokenList.cons (opaque a.name) $ a.value # foldMap \v ->
      TokenList.cons (opaque v.equals) case v.value of
        AttrString t -> TokenList.singleton (opaque t)
        AttrHole h -> holeTokens h
        AttrRaw t -> TokenList.singleton (opaque t)

  childTokens = case _ of
    ChildText t -> TokenList.singleton (opaque t)
    ChildMarkup m -> markupTokens m
    ChildComment t -> TokenList.singleton (opaque t)
    ChildHole h -> holeTokens h
    ChildRaw t -> TokenList.singleton (opaque t)

  holeTokens h =
    TokenList.cons (opaque h.open) (tokensOf h.expr <> TokenList.singleton h.close)

markupRange :: forall e. Markup e -> SourceRange
markupRange = case _ of
  MarkupElement el ->
    { start: el.open.range.start
    , end: maybe el.end.range.end (\c -> maybe c.name.range.end _.range.end c.end) el.close
    }
  MarkupFragment fr ->
    { start: fr.open.range.start
    , end: maybe (maybe fr.open.range.end childEnd (Array.last fr.children)) _.range.end fr.close
    }
  where
  childEnd = case _ of
    ChildText t -> t.range.end
    ChildMarkup m -> (markupRange m).end
    ChildComment t -> t.range.end
    ChildHole h -> h.close.range.end
    ChildRaw t -> t.range.end

instance TokensOf Psx where
  tokensOf = case _ of
    PsxRecovered err -> tokensOf err
    PsxMarkup root ->
      TokenList.uncons' mempty
        (\head tail -> TokenList.cons (head { leadingComments = root.leadingComments <> head.leadingComments }) tail)
        (markupTokens root.markup)

instance RangeOf Psx where
  rangeOf = case _ of
    PsxRecovered err -> rangeOf err
    PsxMarkup root -> markupRange root.markup
