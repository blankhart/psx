-- | Folds over the markup in a module. cst-parser's traversals don't look
-- | inside error payloads, so these recurse into holes themselves.
module Psx.Cst.Traversal
  ( foldMapMarkupRoots
  , foldMapMarkupRootsExpr
  , foldMapHoles
  , markupErrors
  ) where

import Prelude
import Prim hiding (Type)

import Data.Foldable (foldMap)
import Psx.Cst.Types (Attr(..), AttrValue(..), Child(..), Hole, Markup(..), MarkupRoot, Psx(..), PsxError)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapExpr, foldMapModule)
import PureScript.CST.Types (Binder, Declaration, Expr(..), Module, Type)

-- | Every markup literal in the module, outermost first, including those in
-- | holes.
foldMapMarkupRoots :: forall m. Monoid m => (MarkupRoot -> m) -> Module Psx -> m
foldMapMarkupRoots k = foldMapModule (visitor k)

foldMapMarkupRootsExpr :: forall m. Monoid m => (MarkupRoot -> m) -> Expr Psx -> m
foldMapMarkupRootsExpr k = foldMapExpr (visitor k)

visitor
  :: forall m
   . Monoid m
  => (MarkupRoot -> m)
  -> { onBinder :: Binder Psx -> m, onDecl :: Declaration Psx -> m, onExpr :: Expr Psx -> m, onType :: Type Psx -> m }
visitor k = defaultMonoidalVisitor
  { onExpr = case _ of
      ExprError (PsxMarkup root) -> k root <> foldMapHoles (foldMapMarkupRootsExpr k <<< _.expr) root.markup
      _ -> mempty
  }

-- | The holes directly in a markup tree (not those in markup nested in them).
foldMapHoles :: forall m. Monoid m => (Hole Psx -> m) -> Markup Psx -> m
foldMapHoles k = case _ of
  MarkupElement el -> foldMap attr el.attrs <> foldMap child el.children
  MarkupFragment fr -> foldMap child fr.children
  where
  attr = case _ of
    AttrNamed a -> a.value # foldMap \v -> case v.value of
      AttrHole h -> k h
      _ -> mempty
    AttrTrait h -> k h
  child = case _ of
    ChildHole h -> k h
    ChildMarkup m -> foldMapHoles k m
    _ -> mempty

-- | The errors found scanning markup, in source order of their roots.
markupErrors :: Module Psx -> Array PsxError
markupErrors = foldMapMarkupRoots _.errors
