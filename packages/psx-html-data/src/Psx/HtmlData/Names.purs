-- | How HTML and SVG names become PureScript identifiers.
-- |
-- | The generator applies these rules once, resolves clashes between
-- | elements and attributes, and records the result in the table, so tools
-- | read each name's identifier from the table rather than recomputing it.
module Psx.HtmlData.Names
  ( camelCase
  , escapeKeyword
  , identifier
  , keywords
  , signatureNames
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Pattern (Pattern(..))

-- | Words PureScript reserves, which no identifier may be.
keywords :: Array String
keywords =
  [ "ado", "case", "class", "data", "derive", "do", "else", "false", "forall"
  , "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance"
  , "let", "module", "newtype", "of", "then", "true", "type", "where"
  ]

-- | Names the backend signature defines by hand. A generated name that
-- | would equal one of these takes a suffix instead.
signatureNames :: Array String
signatureNames =
  [ "attribute", "children", "element", "keyed", "none", "root", "text" ]

-- | Joins the parts of a name split at `-` and `:`, capitalising every part
-- | but the first: `accept-charset` to `acceptCharset`, `xlink:href` to
-- | `xlinkHref`. Names without separators, such as `viewBox`, are unchanged.
camelCase :: String -> String
camelCase name =
  case Array.uncons (Array.filter (_ /= "") (splitAll name)) of
    Nothing -> name
    Just { head, tail } -> head <> String.joinWith "" (map capitalise tail)
  where
  splitAll = Array.concatMap (String.split (Pattern ":")) <<< String.split (Pattern "-")

-- | `class` becomes `class_`; every other keyword takes a prime, as `type`
-- | becomes `type'`.
escapeKeyword :: String -> String
escapeKeyword name
  | name == "class" = "class_"
  | Array.elem name keywords = name <> "'"
  | otherwise = name

-- | The identifier for a name, before clashes are resolved.
identifier :: String -> String
identifier = escapeKeyword <<< camelCase

capitalise :: String -> String
capitalise s = case CodeUnits.uncons s of
  Nothing -> s
  Just { head, tail } -> String.toUpper (CodeUnits.singleton head) <> tail
