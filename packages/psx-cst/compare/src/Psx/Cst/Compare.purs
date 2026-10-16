-- | `spago run -p psx-cst-compare -- <dir>...` parses every `.purs` file
-- | under the directories with both cst-parser and psx-cst, and fails unless
-- | the two give the same tree (or the same errors) for each.
-- | `fetch-package-set.sh` downloads the pinned package set to compare
-- | against.
module Psx.Cst.Compare (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Psx.Cst (Psx, PsxError, parseModule)
import PureScript.CST as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types (Module)

main :: Effect Unit
main = do
  dirs <- Array.drop 2 <$> Process.argv
  when (Array.null dirs) do
    log "usage: psx-cst-compare <dir>..."
    Process.setExitCode 2
  files <- Array.concat <$> for dirs walk
  mismatches <- Array.catMaybes <$> for files \file -> do
    input <- FS.readTextFile UTF8 file
    pure $ (\reason -> file <> ": " <> reason) <$> compareParsers input
  for_ mismatches log
  log $ show (Array.length files) <> " files, " <> show (Array.length mismatches) <> " differ"
  unless (Array.null mismatches) (Process.setExitCode 1)

-- | Why the two parsers disagree on a source, if they do.
compareParsers :: String -> Maybe String
compareParsers input = do
  let psx = parseModule input
  case CST.parseModule input, psx.module of
    CST.ParseSucceeded m, Just m'
      | not (Array.null psx.errors) -> Just ("psx-cst reports errors: " <> showErrors psx.errors)
      | sameTree m' m -> Nothing
      | otherwise -> Just "trees differ"
    CST.ParseSucceededWithErrors m errs, Just m'
      | map fromCst (Array.fromFoldable errs) /= map key psx.errors -> Just "errors differ"
      | sameTree m' m -> Nothing
      | otherwise -> Just "recovered trees differ"
    CST.ParseFailed err, Nothing
      | [ fromCst err ] == map key psx.errors -> Nothing
      | otherwise -> Just "errors differ"
    CST.ParseFailed err, Just _ -> Just ("cst-parser fails (" <> printParseError err.error <> ") but psx-cst parses")
    _, Nothing -> Just ("psx-cst fails: " <> showErrors psx.errors)
  where
  fromCst :: PositionedError -> { line :: Int, column :: Int, message :: String }
  fromCst e = { line: e.position.line, column: e.position.column, message: printParseError e.error }
  key :: PsxError -> { line :: Int, column :: Int, message :: String }
  key e = { line: e.range.start.line, column: e.range.start.column, message: e.message }
  showErrors es = String.joinWith "; " (map (\e -> show (e.range.start.line + 1) <> ":" <> show (e.range.start.column + 1) <> " " <> e.message) es)

foreign import sameTreeImpl :: forall a b. a -> b -> Boolean

sameTree :: forall a. Module Psx -> Module a -> Boolean
sameTree = sameTreeImpl

walk :: FilePath -> Effect (Array FilePath)
walk dir = do
  names <- Array.sort <$> FS.readdir dir
  Array.concat <$> for names \n -> do
    let p = Path.concat [ dir, n ]
    stats <- FS.lstat p
    if Stats.isDirectory stats then walk p
    else pure (if Stats.isFile stats && Path.extname p == ".purs" then [ p ] else [])
