-- | The psx command line.
-- |
-- | - `psx build [dir...]` lowers every `.psx` file under the directories
-- |   (default `src`) to a `.purs` file beside it, writing only files whose
-- |   text changes. It never overwrites a `.purs` file psx did not write.
-- | - `psx lower <file.psx> [-o <out.purs>] [--map <out.map.json>]` lowers one
-- |   file, to standard output unless `-o` is given.
-- | - `psx check [dir...] [--json] [-- <spago build arguments>]` builds, then
-- |   compiles with `spago build --json-errors` and reports the compiler's
-- |   errors and warnings at their `.psx` positions.
-- |
-- | Compiling stays spago's job: a project runs `psx build && spago build`.
module Psx.Cli
  ( main
  , run
  ) where

import Prelude

import Data.Argonaut.Core (fromArray, stringify)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Psx.Cli.Check (Diagnostic, Severity(..), diagnosticJson, parseCompilerOutput, printDiagnostic, remap)
import Psx.Cli.Process (runSync)
import Psx.Cst (PsxError)
import Psx.Lower (Lowered, isGenerated, lower)
import Psx.Lower.PositionMap (printJson)

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  status <- run args
  Process.setExitCode status

usage :: String
usage = String.joinWith "\n"
  [ "usage: psx build [dir...]"
  , "       psx lower <file.psx> [-o <out.purs>] [--map <out.map.json>]"
  , "       psx check [dir...] [--json] [-- <spago build arguments>]"
  ]

-- | Runs a command and returns its exit status.
run :: Array String -> Effect Int
run args = case Array.uncons args of
  Just { head: "build", tail } -> build (dirs tail)
  Just { head: "lower", tail } -> lowerCommand tail
  Just { head: "check", tail } -> do
    let
      { before, after } = splitDashes tail
      json = Array.elem "--json" before
    check (dirs (Array.filter (_ /= "--json") before)) json after
  _ -> do
    error usage
    pure 2
  where
  dirs ds = if Array.null ds then [ "src" ] else ds

splitDashes :: Array String -> { before :: Array String, after :: Array String }
splitDashes args = case Array.elemIndex "--" args of
  Just i -> { before: Array.take i args, after: Array.drop (i + 1) args }
  Nothing -> { before: args, after: [] }

-- Lowering files ---------------------------------------------------------------

-- | The generated file for a source: `Counter.psx` gives `Counter.purs`.
generatedPath :: FilePath -> FilePath
generatedPath file = Path.concat [ Path.dirname file, Path.basenameWithoutExt file ".psx" <> ".purs" ]

lowerFile :: FilePath -> Effect (Either (Array PsxError) Lowered)
lowerFile file = do
  text <- FS.readTextFile UTF8 file
  pure $ lower
    { sourceName: Path.basename file, generatedName: Path.basename (generatedPath file) }
    text

printErrors :: FilePath -> Array PsxError -> Effect Unit
printErrors file errors = for_ errors \e ->
  error $ file <> ":" <> show (e.range.start.line + 1) <> ":" <> show (e.range.start.column + 1) <> ": error: "
    <> e.message

-- | Every `.psx` file under the directories, skipping dependencies, build
-- | output and hidden directories.
findSources :: Array FilePath -> Effect (Array FilePath)
findSources roots = Array.sort <<< Array.concat <$> traverse walk roots
  where
  walk path = do
    exists <- FS.exists path
    if not exists then pure []
    else do
      stats <- FS.stat path
      if Stats.isDirectory stats then do
        names <- FS.readdir path
        let keep n = not (String.take 1 n == "." || Array.elem n [ "node_modules", "output", ".spago" ])
        Array.concat <$> for (Array.filter keep names) \n -> walk (Path.concat [ path, n ])
      else pure (if Path.extname path == ".psx" then [ path ] else [])

-- | Lowers the sources under the directories and writes what changed.
-- | Returns the number of files that failed.
build :: Array FilePath -> Effect Int
build roots = do
  sources <- findSources roots
  failures <- for sources \file -> do
    result <- lowerFile file
    case result of
      Left errors -> do
        printErrors file errors
        pure 1
      Right lowered -> do
        let out = generatedPath file
        exists <- FS.exists out
        current <- if exists then Just <$> FS.readTextFile UTF8 out else pure Nothing
        case current of
          Just text | not (isGenerated text) -> do
            error $ out <> ": error: not written by psx, so psx build leaves it alone; move it to lower " <> file
            pure 1
          Just text | text == lowered.purs -> pure 0
          _ -> do
            FS.writeTextFile UTF8 out lowered.purs
            log $ "lowered " <> file
            pure 0
  let failed = Array.foldl (+) 0 failures
  pure (if failed > 0 then 1 else 0)

lowerCommand :: Array String -> Effect Int
lowerCommand args = case args of
  [ file ] -> one file Nothing Nothing
  _ -> case Array.uncons args of
    Just { head: file, tail } -> one file (option "-o" tail) (option "--map" tail)
    Nothing -> do
      error usage
      pure 2
  where
  option name opts = Array.elemIndex name opts >>= \i -> Array.index opts (i + 1)

  one file out mapOut = do
    result <- lowerFile file
    case result of
      Left errors -> do
        printErrors file errors
        pure 1
      Right lowered -> do
        case out of
          Just path -> FS.writeTextFile UTF8 path lowered.purs
          Nothing -> log (String.trim lowered.purs)
        for_ mapOut \path -> FS.writeTextFile UTF8 path (printJson lowered.positionMap)
        pure 0

-- Checking -----------------------------------------------------------------------

check :: Array FilePath -> Boolean -> Array String -> Effect Int
check roots json spagoArgs = do
  built <- build roots
  if built /= 0 then pure built
  else do
    spago <- fromMaybe "npx spago" <$> Process.lookupEnv "PSX_SPAGO"
    let
      command = String.split (String.Pattern " ") spago
      exe = fromMaybe "npx" (Array.head command)
    result <- runSync exe (Array.drop 1 command <> [ "build", "--json-errors" ] <> spagoArgs)
    let diagnostics = parseCompilerOutput result.stdout <> parseCompilerOutput result.stderr
    remapped <- traverse remapDiagnostic diagnostics
    if json then log (stringify (fromArray (map diagnosticJson remapped)))
    else for_ remapped (error <<< printDiagnostic)
    let errors = Array.filter (\d -> d.severity == Error) remapped
    when (result.status /= 0 && Array.null errors) do
      -- spago failed before the compiler reported anything.
      error (result.stderr <> result.stdout)
    pure (if result.status /= 0 || not (Array.null errors) then 1 else 0)

-- | A diagnostic in a file psx generated, moved to its source.
remapDiagnostic :: Diagnostic -> Effect Diagnostic
remapDiagnostic d = case d.file of
  Nothing -> pure d
  Just file -> do
    exists <- FS.exists file
    text <- if exists then FS.readTextFile UTF8 file else pure ""
    let named = String.stripPrefix (String.Pattern "-- Generated by psx from ") (String.takeWhile (_ /= String.codePointFromChar '\n') text)
    case named of
      Just name | isGenerated text -> do
        let sourcePath = Path.concat [ Path.dirname file, name ]
        sourceExists <- FS.exists sourcePath
        if not sourceExists then pure d
        else do
          result <- lowerFile sourcePath
          pure case result of
            Right lowered -> remap { generated: file, source: sourcePath, positionMap: lowered.positionMap } d
            Left _ -> d
      _ -> pure d
