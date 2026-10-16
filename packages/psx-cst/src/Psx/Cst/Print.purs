-- | The exact printer: printing a parsed module gives back its source.
module Psx.Cst.Print
  ( printModule
  , printMarkup
  , printExpr
  ) where

import Prelude

import Data.Foldable (foldMap)
import Psx.Cst.Types (Markup, Psx, markupTokens)
import PureScript.CST as CST
import PureScript.CST.Print (printSourceToken)
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Expr, Module)

printModule :: Module Psx -> String
printModule = CST.printModule

-- | A markup tree, without the comments before its root.
printMarkup :: Markup Psx -> String
printMarkup = foldMap printSourceToken <<< TokenList.toArray <<< markupTokens

printExpr :: Expr Psx -> String
printExpr = foldMap printSourceToken <<< TokenList.toArray <<< tokensOf
