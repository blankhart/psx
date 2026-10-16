-- | The corpus runner. A case is a directory `test/corpus/<area>/<case>/`
-- | holding `input.psx` or `input.psxt`. Each stage that applies to a case
-- | produces one golden file, `expected.<ext>`, beside the input.
module Psx.Corpus
  ( Case
  , Stage
  , Options
  , parseArgs
  , findCorpus
  , discover
  , runCorpus
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path

type Case =
  { area :: String
  , name :: String
  , dir :: FilePath
  -- | `input.psx` or `input.psxt`
  , inputFile :: String
  , input :: String
  }

-- | `golden` is the file the stage writes in the case directory, such as
-- | `expected.purs`. `run` returns the golden's contents.
type Stage =
  { name :: String
  , golden :: String
  , applies :: Case -> Boolean
  , run :: Case -> Effect String
  }

type Options =
  { accept :: Boolean
  -- | `area` or `area/case`; empty means every case
  , only :: Array String
  }

parseArgs :: Array String -> Options
parseArgs args =
  { accept: Array.elem "--accept" args
  , only: Array.filter (not <<< String.contains (String.Pattern "--")) args
  }

-- | The nearest `test/corpus` directory at or above the given directory.
findCorpus :: FilePath -> Effect (Maybe FilePath)
findCorpus dir = do
  let candidate = Path.concat [ dir, "test", "corpus" ]
  found <- FS.exists candidate
  if found then pure (Just candidate)
  else do
    let parent = Path.dirname dir
    if parent == dir then pure Nothing else findCorpus parent

subdirs :: FilePath -> Effect (Array String)
subdirs dir = do
  names <- Array.sort <$> FS.readdir dir
  Array.filterA (\n -> Stats.isDirectory <$> FS.stat (Path.concat [ dir, n ])) names

discover :: FilePath -> Effect (Array Case)
discover corpus = do
  areas <- subdirs corpus
  Array.concat <$> for areas \area -> do
    names <- subdirs (Path.concat [ corpus, area ])
    Array.catMaybes <$> for names \name -> do
      let dir = Path.concat [ corpus, area, name ]
      files <- FS.readdir dir
      case Array.find (_ `Array.elem` files) [ "input.psx", "input.psxt" ] of
        Nothing -> pure Nothing
        Just inputFile -> do
          input <- FS.readTextFile UTF8 (Path.concat [ dir, inputFile ])
          pure (Just { area, name, dir, inputFile, input })

selected :: Options -> Case -> Boolean
selected { only } c = Array.null only || Array.elem c.area only || Array.elem (c.area <> "/" <> c.name) only

-- | Checks (or with `accept`, rewrites) every golden. Returns the number of
-- | failures: a golden that differs, is missing, or that no stage produces.
runCorpus :: Array Stage -> Options -> FilePath -> Effect Int
runCorpus stages opts corpus = do
  cases <- Array.filter (selected opts) <$> discover corpus
  results <- traverse runCase cases
  let failures = Array.foldl (+) 0 results
  log $ count (Array.length cases) "case" <> ", " <> count failures "failure"
    <> (if opts.accept then " (goldens rewritten)" else "")
  pure failures
  where
  runCase c = do
    let applicable = Array.filter (\s -> s.applies c) stages
    produced <- traverse (runStage c) applicable
    orphans <- orphanGoldens c (map _.golden applicable)
    for_ orphans \g ->
      if opts.accept then do
        FS.unlink (Path.concat [ c.dir, g ])
        log $ "removed " <> label c g
      else log $ "FAIL " <> label c g <> ": no stage produces this golden"
    pure (Array.foldl (+) 0 produced + if opts.accept then 0 else Array.length orphans)

  runStage c stage = do
    actual <- stage.run c
    let path = Path.concat [ c.dir, stage.golden ]
    exists <- FS.exists path
    expected <- if exists then Just <$> FS.readTextFile UTF8 path else pure Nothing
    if expected == Just actual then pure 0
    else if opts.accept then do
      FS.writeTextFile UTF8 path actual
      log $ "wrote " <> label c stage.golden
      pure 0
    else do
      case expected of
        Nothing -> log $ "FAIL " <> label c stage.golden <> ": missing (run with --accept)"
        Just e -> do
          log $ "FAIL " <> label c stage.golden <> ": differs"
          traverse_ log (diffLines e actual)
      pure 1

  label c golden = c.area <> "/" <> c.name <> "/" <> golden
  count n noun = show n <> " " <> noun <> if n == 1 then "" else "s"

orphanGoldens :: Case -> Array String -> Effect (Array String)
orphanGoldens c goldens = do
  files <- FS.readdir c.dir
  pure $ Array.sort $ Array.filter (\f -> String.take 9 f == "expected." && not (Array.elem f goldens)) files

-- | The first differing line, with line numbers, enough to find the change.
diffLines :: String -> String -> Array String
diffLines expected actual = do
  let
    es = String.split (String.Pattern "\n") expected
    as = String.split (String.Pattern "\n") actual
    go i = case Array.index es i, Array.index as i of
      Nothing, Nothing -> []
      e, a
        | e == a -> go (i + 1)
        | otherwise ->
            [ "  line " <> show (i + 1) <> ":"
            , "    expected: " <> show' e
            , "    actual:   " <> show' a
            ]
    show' = case _ of
      Just s -> show s
      Nothing -> "(end of file)"
  go 0
