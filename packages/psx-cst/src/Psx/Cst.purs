-- | Concrete syntax for `.psx` files: PureScript with markup literals.
-- |
-- | `parseModule` parses a module to cst-parser's tree with `Psx` as the
-- | error payload, where markup literals live (see `Psx.Cst.Types`).
-- | `printModule` prints it back exactly.
module Psx.Cst
  ( module Psx.Cst.Types
  , module Psx.Cst.Parser
  , module Psx.Cst.Print
  , module Psx.Cst.Traversal
  ) where

import Psx.Cst.Parser (ParseResult, parseModule)
import Psx.Cst.Print (printExpr, printMarkup, printModule)
import Psx.Cst.Traversal (foldMapHoles, foldMapMarkupRoots, foldMapMarkupRootsExpr, markupErrors)
import Psx.Cst.Types (Attr(..), AttrValue(..), Child(..), Edit, Element, ElementClose, Fix, Fragment, Hole, MToken, Markup(..), MarkupRoot, NamedAttr, Psx(..), PsxError, markupTokens, mtokenSource, namedAttrs)
