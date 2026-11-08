-- | The lowering stages: `expected.purs` is the lowered module, or the
-- | errors that stopped it, `expected.map.json` its position map and
-- | `expected.css` its stylesheet. A case with `same-as` also gets
-- | `expected.same`, which records whether it lowers to the same code as the
-- | case it names. Corpus cases under `parse/` are left to the parse stage,
-- | and those under `fmt/` to the format stage.
module Psx.Corpus.Lower (lower, positionMap, css, same, warnings, lowerCase) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Tuple (Tuple(..))
import Psx.Corpus (Case, Stage)
import Psx.Cst (PsxError)
import Psx.Lower (Lowered)
import Psx.Lower as Lower
import Psx.Lower.PositionMap as PositionMap
import PureScript.CST.Lexer (lexModule)
import PureScript.CST.Print (printToken)
import PureScript.CST.TokenStream (TokenStep(..))
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Token(..))

applies :: Case -> Boolean
applies c = c.area /= "parse" && c.area /= "fmt"

lowerCase :: { inputFile :: String, input :: String } -> Either (Array PsxError) Lowered
lowerCase c
  | c.inputFile == "input.psxt" = Lower.lowerPsxt { sourceName: c.inputFile, generatedName: "input.purs" } c.input
  | otherwise = Lower.lower { sourceName: c.inputFile, generatedName: "input.purs" } c.input

lowered :: Case -> Either (Array PsxError) Lowered
lowered c = lowerCase { inputFile: c.inputFile, input: c.input }

lower :: Stage
lower =
  { name: "lower"
  , golden: "expected.purs"
  , applies
  , run: \c -> pure case lowered c of
      Right result -> result.purs
      Left errors -> "-- psx lower failed\n" <> foldMap error errors
  }
  where
  error e =
    "-- " <> show (e.range.start.line + 1) <> ":" <> show (e.range.start.column + 1)
      <> " "
      <> e.message
      <> "\n"

positionMap :: Stage
positionMap =
  { name: "position map"
  , golden: "expected.map.json"
  , applies: \c -> applies c && case lowered c of
      Right _ -> true
      Left _ -> false
  , run: \c -> pure case lowered c of
      Right result -> PositionMap.printJson result.positionMap
      Left _ -> ""
  }

css :: Stage
css =
  { name: "css"
  , golden: "expected.css"
  , applies: \c -> applies c && case lowered c of
      Right result -> isJust result.style
      Left _ -> false
  , run: \c -> pure case lowered c of
      Right { style: Just style } -> style.css
      _ -> ""
  }

-- | The two lowerings' tokens, with the layout cst-parser infers as
-- | explicit braces and semicolons and without comments, so the header's
-- | file name and line breaks don't count; and their stylesheets, without
-- | the header line.
same :: Stage
same =
  { name: "same code"
  , golden: "expected.same"
  , applies: \c -> applies c && isJust c.sameAs
  , run: \c -> pure case c.sameAs of
      Nothing -> ""
      Just other -> case lowered c, lowerCase { inputFile: other.inputFile, input: other.input } of
        Right a, Right b -> do
          let
            ta = tokens a.purs
            tb = tokens b.purs
            cssOf l = foldMap (\s -> String.joinWith "\n" (Array.drop 1 (String.split (String.Pattern "\n") s.css))) l.style
          case Array.findIndex (\(Tuple x y) -> x /= y) (Array.zip ta tb) of
            Nothing
              | Array.length ta == Array.length tb ->
                  "same code as " <> other.name <> ": " <> show (Array.length ta) <> " tokens\n"
                    <> (if cssOf a == cssOf b then "same stylesheet\n" else "stylesheets differ\n")
            found -> do
              let
                i = case found of
                  Just n -> n
                  Nothing -> min (Array.length ta) (Array.length tb)
                around ts = String.joinWith " " (Array.slice (max 0 (i - 5)) (i + 5) ts)
              "differs from " <> other.name <> " at token " <> show (i + 1) <> "\n  here:  " <> around ta <> "\n  there: " <> around tb <> "\n"
        _, _ -> "one of the two does not lower\n"
  }

tokens :: String -> Array String
tokens text = go [] (lexModule text)
  where
  go acc stream = case TokenStream.step stream of
    TokenCons tok _ next _ -> go (Array.snoc acc (show' tok.value)) next
    _ -> acc
  show' = case _ of
    TokLayoutStart _ -> "{"
    TokLayoutSep _ -> ";"
    TokLayoutEnd _ -> "}"
    t -> printToken t

warnings :: Stage
warnings =
  { name: "warnings"
  , golden: "expected.warnings"
  , applies: \c -> applies c && case lowered c of
      Right result -> not (Array.null result.warnings)
      Left _ -> false
  , run: \c -> pure case lowered c of
      Right result -> foldMap (\w -> show (w.range.start.line + 1) <> ":" <> show (w.range.start.column + 1) <> ": " <> w.message <> "\n") result.warnings
      Left _ -> ""
  }
