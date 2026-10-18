-- | The comparison report: one row per scenario, framework and variant, with
-- | the measures the build plan lists.
-- |
-- | - **Pass**: every step and every DOM contract.
-- | - **Source size**: lines and tokens of the app's own source, comments
-- |   and blank lines left out, and generated `.purs` too.
-- | - **Cross-framework diff**: lines added and removed between this app's
-- |   source and the same variant of the scenario on each other framework.
-- | - **Bundle size**: esbuild minified, then gzip, in bytes, from purs's
-- |   own code generator and from purs-backend-es. The bundles section sets
-- |   each framework's `psx/` against its `purs/` under both, so the second
-- |   gap shows what a general optimiser leaves of the backend's dispatch.
-- | - **Runtime**: the median of each `Measure`'s runs, in milliseconds,
-- |   with its quartiles as the spread.
-- | - **Build time**: `psx build` of the app, and `spago build` of its
-- |   package with the app's own modules removed from `output/`, in
-- |   seconds (with `--build-time`).
-- |
-- | With `--control`, each framework's `purs/` app runs twice, and the
-- | timings section checks every `psx/` median against that run-to-run
-- | noise: the difference between the two `purs/` medians, or the pooled
-- | `purs/` interquartile range, whichever is larger. A `psx/` median is
-- | within noise unless it is slower than `purs/` by more than that; one
-- | faster by more says the noise was underestimated, not that `psx/`
-- | costs anything. With `--history`, it
-- | gives each median's change from the last recorded run.
-- |
-- | It writes `report.json`, `report.md` (the job summary in CI) and
-- | `index.html`, and with `--history`, `history.jsonl`.
module Suite.Report
  ( AppResult
  , Config
  , write
  , tokens
  , changedLines
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJsonNumber, caseJsonString, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, stringify, toArray, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (foldMap, foldl, sum)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (fixed, toStringWith)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path as Path
import Suite.Stats (Summary, summarise)

-- | What running one app gave. `control` marks the second run of a
-- | framework's `purs/` app.
type AppResult =
  { scenario :: String
  , framework :: String
  , variant :: String
  , control :: Boolean
  , pass :: Boolean
  , problems :: Array String
  , measures :: Array { name :: String, samples :: Array Number }
  , bundle :: Maybe { bytes :: Int, gzip :: Int }
  -- | The app bundled from purs-backend-es's output, not run.
  , esBundle :: Maybe { bytes :: Int, gzip :: Int }
  }

type Config =
  { root :: String
  , out :: String
  , buildTime :: Boolean
  , history :: Maybe String
  }

type Source = { files :: Array String, lines :: Int, tokens :: Int, text :: Array String }

type Build = { psx :: Maybe Number, spago :: Number }

type ReportRow =
  { result :: AppResult
  , source :: Source
  , diff :: Array { framework :: String, lines :: Int }
  , runtime :: Array { name :: String, summary :: Summary }
  , build :: Maybe Build
  }

-- | A `psx/` median against its framework's `purs/` medians.
type Timing =
  { scenario :: String
  , framework :: String
  , name :: String
  , purs :: Summary
  , psx :: Summary
  , control :: Maybe Summary
  , noise :: Maybe Number
  , within :: Maybe Boolean
  , previous :: { purs :: Maybe Number, psx :: Maybe Number }
  }

-- | Writes the report. Its result names every `psx/` timing outside the
-- | noise.
write :: Config -> Array AppResult -> Effect { outsideNoise :: Array String }
write config results = do
  let apps = Array.filter (not <<< _.control) results
  sources <- for apps \r -> Tuple r <$> source config.root r
  builds <- if config.buildTime then for apps (map Just <<< buildTime config.root) else pure (map (const Nothing) apps)
  previous <- maybe (pure Nothing) readLast config.history
  let
    rows = Array.zipWith
      ( \(Tuple r s) build ->
          { result: r
          , source: s
          , diff: Array.mapMaybe
              ( \(Tuple o os) ->
                  if o.scenario == r.scenario && o.variant == r.variant && o.framework /= r.framework then
                    Just { framework: o.framework, lines: changedLines s.text os.text }
                  else Nothing
              )
              sources
          , runtime: map (\m -> { name: m.name, summary: summarise m.samples }) r.measures
          , build
          }
      )
      sources
      builds
    timings = timingsOf results previous
    outsideNoise = Array.mapMaybe
      ( \t -> case t.within of
          Just false -> Just
            ( t.scenario <> "/" <> t.framework <> " " <> show t.name <> ": psx " <> ms t.psx.median <> ", purs "
                <> ms t.purs.median
                <> ", noise "
                <> maybe "?" ms t.noise
            )
          _ -> Nothing
      )
      timings
    json = reportJson rows timings
  FS.mkdir' config.out { recursive: true, mode: permsAll }
  FS.writeTextFile UTF8 (Path.concat [ config.out, "report.json" ]) (stringify json <> "\n")
  FS.writeTextFile UTF8 (Path.concat [ config.out, "report.md" ]) (markdown rows timings)
  FS.writeTextFile UTF8 (Path.concat [ config.out, "index.html" ]) (html rows timings)
  for_' config.history \file -> do
    exists <- FS.exists file
    old <- if exists then FS.readTextFile UTF8 file else pure ""
    stamp <- runStamp
    let line = stringify (historyLine stamp results)
    FS.writeTextFile UTF8 (Path.concat [ config.out, "history.jsonl" ]) (old <> (if old == "" || CodeUnits.takeRight 1 old == "\n" then "" else "\n") <> line <> "\n")
  pure { outsideNoise }
  where
  for_' m f = maybe (pure unit) f m

-- Source size and diffs

-- | The app's own source files: `.psx`, `.psxt` and `.purs`, but not the
-- | `.purs` the lowering generates.
source :: String -> AppResult -> Effect Source
source root r = do
  let dir = Path.concat [ root, "test/suite/apps", r.scenario, r.framework, r.variant, "src" ]
  files <- Array.sort <$> walk dir
  own <- Array.filterA hand files
  texts <- traverse (FS.readTextFile UTF8) own
  let
    code = Array.concatMap codeLines texts
  pure
    { files: map (Path.relative root) own
    , lines: Array.length code
    , tokens: sum (map (Array.length <<< tokens) code)
    , text: code
    }
  where
  hand file
    | Array.any (\ext -> CodeUnits.takeRight (String.length ext) file == ext) [ ".psx", ".psxt" ] = pure true
    | CodeUnits.takeRight 5 file == ".purs" = do
        text <- FS.readTextFile UTF8 file
        pure (not (String.contains (String.Pattern "-- Generated by psx") (String.take 80 text)))
    | otherwise = pure false

walk :: String -> Effect (Array String)
walk dir = do
  exists <- FS.exists dir
  if not exists then pure []
  else do
    names <- FS.readdir dir
    Array.concat <$> for names \name -> do
      let path = Path.concat [ dir, name ]
      stats <- FS.stat path
      if Stats.isDirectory stats then walk path else pure [ path ]

-- | The lines that hold code, trimmed, with comments removed.
codeLines :: String -> Array String
codeLines text = Array.filter (_ /= "") $ map String.trim $ String.split (String.Pattern "\n") (stripComments text)

-- | Removes `--` line comments and `{- -}` block comments, but not inside
-- | string literals.
stripComments :: String -> String
stripComments text = String.joinWith "" (Array.fromFoldable (List.reverse (go 0 0 Nil)))
  where
  n = CodeUnits.length text
  startsAt i s = CodeUnits.slice i (i + CodeUnits.length s) text == s
  keep from i acc = CodeUnits.slice from i text : acc
  go from i acc
    | i >= n = keep from n acc
    | startsAt i "\"\"\"" = go from (maybe n (_ + 3) (CodeUnits.indexOf' (String.Pattern "\"\"\"") (i + 3) text)) acc
    | startsAt i "--" =
        let end = fromMaybe n (CodeUnits.indexOf' (String.Pattern "\n") i text)
        in go end end (keep from i acc)
    | startsAt i "{-" =
        let
          end = maybe n (_ + 2) (CodeUnits.indexOf' (String.Pattern "-}") i text)
          newlines = CodeUnits.fromCharArray (Array.filter (_ == '\n') (CodeUnits.toCharArray (CodeUnits.slice i end text)))
        in
          go end end (newlines : keep from i acc)
    | CodeUnits.charAt i text == Just '"' = go from (1 + i + stringLength (CodeUnits.toCharArray (CodeUnits.slice (i + 1) (min n (i + 1000)) text))) acc
    | otherwise = go from (i + 1) acc

-- | The length of a string literal's body and closing quote.
stringLength :: Array Char -> Int
stringLength cs = go 0
  where
  go i = case Array.index cs i of
    Nothing -> i
    Just '\\' -> go (i + 2)
    Just '"' -> i + 1
    Just '\n' -> i
    Just _ -> go (i + 1)

data Class = Space | Word | Digit | Symbol | Quote | Other

derive instance Eq Class

classify :: Char -> Class
classify c
  | c == ' ' || c == '\t' || c == '\r' || c == '\n' = Space
  | c == '"' = Quote
  | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '\'' = Word
  | c >= '0' && c <= '9' = Digit
  | CodeUnits.contains (String.Pattern (CodeUnits.singleton c)) ":!#$%&*+./<=>?@\\^|-~" = Symbol
  | otherwise = Other

-- | A line's tokens: identifiers (with their digits and primes), numbers,
-- | string literals, runs of operator characters, and each other character
-- | on its own.
tokens :: String -> Array String
tokens line = go (CodeUnits.toCharArray line) []
  where
  go cs acc = case Array.head cs of
    Nothing -> acc
    Just c -> case classify c of
      Space -> go (Array.drop 1 cs) acc
      Quote ->
        let n = 1 + stringLength (Array.drop 1 cs)
        in go (Array.drop n cs) (Array.snoc acc (CodeUnits.fromCharArray (Array.take n cs)))
      Other -> go (Array.drop 1 cs) (Array.snoc acc (CodeUnits.singleton c))
      cls ->
        let
          same d = case cls, classify d of
            Word, Digit -> true
            Digit, Word -> false
            a, b -> a == b
          n = 1 + Array.length (Array.takeWhile same (Array.drop 1 cs))
        in
          go (Array.drop n cs) (Array.snoc acc (CodeUnits.fromCharArray (Array.take n cs)))

-- | Lines added plus lines removed, by the longest common subsequence.
changedLines :: Array String -> Array String -> Int
changedLines as bs = Array.length as + Array.length bs - 2 * lcs
  where
  zeros = map (const 0) (Array.range 0 (Array.length bs))
  lcs = fromMaybe 0 $ Array.last $ foldl row zeros as
  row prev a =
    foldl
      ( \acc (Tuple j b) ->
          let
            left = fromMaybe 0 (Array.last acc)
            up = fromMaybe 0 (Array.index prev (j + 1))
            diag = fromMaybe 0 (Array.index prev j)
          in
            Array.snoc acc (if a == b then diag + 1 else max left up)
      )
      [ 0 ]
      (Array.mapWithIndex Tuple bs)

-- Build time

buildTime :: String -> AppResult -> Effect Build
buildTime root r = do
  let
    appDir = Path.concat [ "test/suite/apps", r.scenario, r.framework, r.variant ]
    package = "psx-suite-" <> r.scenario <> "-" <> r.framework <> "-" <> r.variant
  psx <-
    if r.variant == "psx" then do
      t <- timeCommand root "node" [ "packages/psx-cli/bin/psx.mjs", "build", appDir ]
      when (not t.ok) $ log ("     psx build " <> appDir <> " failed:\n" <> t.output)
      pure (Just t.seconds)
    else pure Nothing
  files <- walk (Path.concat [ root, appDir, "src" ])
  let
    modules = Array.mapMaybe (moduleOf (Path.concat [ root, appDir, "src" ])) files
  for_' modules \m -> removeDir (Path.concat [ root, "output", m ])
  t <- timeCommand root (Path.concat [ root, "node_modules/.bin/spago" ]) [ "build", "-p", package ]
  when (not t.ok) $ log ("     spago build -p " <> package <> " failed:\n" <> t.output)
  log $ "     " <> r.scenario <> "/" <> r.framework <> "/" <> r.variant <> " build: "
    <> maybe "" (\s -> "psx " <> seconds s <> ", ") psx
    <> "spago "
    <> seconds t.seconds
  pure { psx, spago: t.seconds }
  where
  for_' xs f = void (traverse f xs)
  moduleOf src file =
    case String.stripSuffix (String.Pattern ".purs") file of
      Just stem -> Just (String.replaceAll (String.Pattern "/") (String.Replacement ".") (Path.relative src stem))
      Nothing -> Nothing

foreign import timeCommand :: String -> String -> Array String -> Effect { ok :: Boolean, seconds :: Number, output :: String }

foreign import removeDir :: String -> Effect Unit

-- | The commit and time of this run, from the CI environment when there.
foreign import runStamp :: Effect { commit :: String, date :: String, run :: String }

-- Timings and noise

timingsOf :: Array AppResult -> Maybe Json -> Array Timing
timingsOf results previous = do
  purs <- Array.filter (\r -> r.variant == "purs" && not r.control) results
  psx <- Array.filter (\r -> r.variant == "psx" && r.scenario == purs.scenario && r.framework == purs.framework) results
  m <- purs.measures
  psxSamples <- Array.fromFoldable (Array.find (\x -> x.name == m.name) psx.measures)
  let
    control = Array.find (\r -> r.control && r.scenario == purs.scenario && r.framework == purs.framework) results
      >>= \run -> Array.find (\x -> x.name == m.name) run.measures
    p = summarise m.samples
    q = summarise psxSamples.samples
    c = summarise <<< _.samples <$> control
    pooled = summarise <<< (m.samples <> _) <<< _.samples <$> control
    noise = do
      cs <- c
      pl <- pooled
      pure (max (abs (cs.median - p.median)) (pl.q3 - pl.q1))
    prior variant = previous >>= priorMedian { scenario: purs.scenario, framework: purs.framework, variant, name: m.name }
  pure
    { scenario: purs.scenario
    , framework: purs.framework
    , name: m.name
    , purs: p
    , psx: q
    , control: c
    , noise
    , within: (\n -> q.median - p.median <= n) <$> noise
    , previous: { purs: prior "purs", psx: prior "psx" }
    }
  where
  abs x = if x < 0.0 then -x else x

-- History: one JSON object per line, each run's medians.

historyLine :: { commit :: String, date :: String, run :: String } -> Array AppResult -> Json
historyLine stamp results = fromObject $ Object.fromFoldable
  [ Tuple "commit" (fromString stamp.commit)
  , Tuple "date" (fromString stamp.date)
  , Tuple "run" (fromString stamp.run)
  , Tuple "medians" $ fromArray do
      r <- Array.filter (not <<< _.control) results
      m <- r.measures
      let s = summarise m.samples
      pure $ fromObject $ Object.fromFoldable
        [ Tuple "scenario" (fromString r.scenario)
        , Tuple "framework" (fromString r.framework)
        , Tuple "variant" (fromString r.variant)
        , Tuple "name" (fromString m.name)
        , Tuple "median" (fromNumber s.median)
        , Tuple "q1" (fromNumber s.q1)
        , Tuple "q3" (fromNumber s.q3)
        , Tuple "n" (fromNumber (toNumber s.n))
        ]
  ]

-- | The last run in the history file.
readLast :: String -> Effect (Maybe Json)
readLast file = do
  exists <- FS.exists file
  if not exists then pure Nothing
  else do
    text <- FS.readTextFile UTF8 file
    pure $ Array.last (String.split (String.Pattern "\n") text # Array.filter (_ /= "")) >>= (hush <<< jsonParser)

priorMedian :: { scenario :: String, framework :: String, variant :: String, name :: String } -> Json -> Maybe Number
priorMedian key json = do
  o <- toObject json
  medians <- Object.lookup "medians" o >>= toArray
  entry <- Array.find matches medians >>= toObject
  Object.lookup "median" entry >>= caseJsonNumber Nothing Just
  where
  field o k = Object.lookup k o >>= caseJsonString Nothing Just
  matches j = fromMaybe false do
    o <- toObject j
    s <- field o "scenario"
    f <- field o "framework"
    v <- field o "variant"
    n <- field o "name"
    pure (s == key.scenario && f == key.framework && v == key.variant && n == key.name)

-- JSON

reportJson :: Array ReportRow -> Array Timing -> Json
reportJson rows timings = fromObject $ Object.fromFoldable
  [ Tuple "rows" (fromArray (map rowJson rows))
  , Tuple "timings" (fromArray (map timingJson timings))
  ]
  where
  rowJson row = obj
    [ Tuple "scenario" (fromString row.result.scenario)
    , Tuple "framework" (fromString row.result.framework)
    , Tuple "variant" (fromString row.result.variant)
    , Tuple "pass" (fromBoolean row.result.pass)
    , Tuple "problems" (fromArray (map fromString row.result.problems))
    , Tuple "source" $ obj
        [ Tuple "files" (fromArray (map fromString row.source.files))
        , Tuple "lines" (int row.source.lines)
        , Tuple "tokens" (int row.source.tokens)
        ]
    , Tuple "diff" (fromArray (map (\d -> obj [ Tuple "framework" (fromString d.framework), Tuple "lines" (int d.lines) ]) row.diff))
    , Tuple "bundle" (maybe jsonNull sizeJson row.result.bundle)
    , Tuple "esBundle" (maybe jsonNull sizeJson row.result.esBundle)
    , Tuple "runtime" (fromArray (map (\m -> obj [ Tuple "name" (fromString m.name), Tuple "summary" (summaryJson m.summary), Tuple "samples" (samplesOf row m.name) ]) row.runtime))
    , Tuple "build" (maybe jsonNull (\b -> obj [ Tuple "psx" (maybe jsonNull fromNumber b.psx), Tuple "spago" (fromNumber b.spago) ]) row.build)
    ]
  sizeJson b = obj [ Tuple "bytes" (int b.bytes), Tuple "gzip" (int b.gzip) ]
  samplesOf row name = fromArray $ map fromNumber $ fromMaybe [] $ _.samples <$> Array.find (\m -> m.name == name) row.result.measures
  timingJson t = obj
    [ Tuple "scenario" (fromString t.scenario)
    , Tuple "framework" (fromString t.framework)
    , Tuple "name" (fromString t.name)
    , Tuple "purs" (summaryJson t.purs)
    , Tuple "psx" (summaryJson t.psx)
    , Tuple "control" (maybe jsonNull summaryJson t.control)
    , Tuple "noise" (maybe jsonNull fromNumber t.noise)
    , Tuple "within" (maybe jsonNull fromBoolean t.within)
    , Tuple "previous" (obj [ Tuple "purs" (maybe jsonNull fromNumber t.previous.purs), Tuple "psx" (maybe jsonNull fromNumber t.previous.psx) ])
    ]
  summaryJson s = obj
    [ Tuple "n" (int s.n)
    , Tuple "median" (fromNumber s.median)
    , Tuple "q1" (fromNumber s.q1)
    , Tuple "q3" (fromNumber s.q3)
    , Tuple "min" (fromNumber s.min)
    , Tuple "max" (fromNumber s.max)
    ]
  obj = fromObject <<< Object.fromFoldable
  int = fromNumber <<< toNumber

-- Tables, as Markdown and as HTML

type Table = { headers :: Array String, rows :: Array (Array String) }

gridTable :: Array ReportRow -> Table
gridTable rows =
  { headers: [ "Scenario", "Framework", "Variant", "Pass", "Source (lines / tokens)", "Diff from other frameworks (lines)", "Bundle (gzip B)", "Bundle, purs-backend-es (gzip B)", "Runtime (median ms, quartiles)", "Build (s)" ]
  , rows: rows <#> \row ->
      [ row.result.scenario
      , row.result.framework
      , row.result.variant
      , if row.result.pass then "pass" else "FAIL"
      , show row.source.lines <> " / " <> show row.source.tokens
      , String.joinWith ", " (map (\d -> d.framework <> " " <> show d.lines) row.diff)
      , maybe "" (show <<< _.gzip) row.result.bundle
      , maybe "" (show <<< _.gzip) row.result.esBundle
      , runtimeCell row.runtime
      , maybe "" (\b -> maybe "" (\p -> "psx " <> seconds p <> ", ") b.psx <> "spago " <> seconds b.spago) row.build
      ]
  }
  where
  runtimeCell = case _ of
    [] -> ""
    [ m ] -> m.name <> ": " <> spread m.summary
    ms' -> show (Array.length ms') <> " measures, below"

-- | Each framework's `psx/` bundle against its `purs/` one, minified and
-- | gzipped, from purs's code generator and from purs-backend-es.
bundleTable :: Array ReportRow -> Table
bundleTable rows =
  { headers: [ "Scenario", "Framework", "purs/ (B, gzip)", "psx/ (B, gzip)", "psx/ − purs/", "purs/, es (B, gzip)", "psx/, es (B, gzip)", "psx/ − purs/, es" ]
  , rows: Array.mapMaybe pair rows
  }
  where
  pair psx
    | psx.result.variant /= "psx" = Nothing
    | otherwise = do
        purs <- Array.find (\r -> r.result.variant == "purs" && r.result.scenario == psx.result.scenario && r.result.framework == psx.result.framework) rows
        pure
          [ psx.result.scenario
          , psx.result.framework
          , maybe "" size purs.result.bundle
          , maybe "" size psx.result.bundle
          , gap purs.result.bundle psx.result.bundle
          , maybe "" size purs.result.esBundle
          , maybe "" size psx.result.esBundle
          , gap purs.result.esBundle psx.result.esBundle
          ]
  size b = show b.bytes <> ", " <> show b.gzip
  gap (Just a) (Just b) = signedInt (b.bytes - a.bytes) <> ", " <> signedInt (b.gzip - a.gzip)
  gap _ _ = ""
  signedInt n = (if n >= 0 then "+" else "") <> show n

timingTable :: Array Timing -> Table
timingTable timings =
  { headers: [ "Scenario", "Framework", "Measure", "purs/ (ms)", "psx/ (ms)", "psx/ − purs/", "Noise", "Within noise", "Change since last run" ]
  , rows: timings <#> \t ->
      [ t.scenario
      , t.framework
      , t.name
      , spread t.purs
      , spread t.psx
      , signed (t.psx.median - t.purs.median)
      , maybe "" ms t.noise
      , maybe "" (if _ then "yes" else "NO") t.within
      , String.joinWith ", " $ Array.catMaybes
          [ change "purs" t.purs.median <$> t.previous.purs
          , change "psx" t.psx.median <$> t.previous.psx
          ]
      ]
  }
  where
  change variant now before = variant <> " " <> percent ((now - before) / before)

markdown :: Array ReportRow -> Array Timing -> String
markdown rows timings =
  "# PSX suite report\n\n"
    <> summaryLine rows timings
    <> "\n\n"
    <> mdTable (gridTable rows)
    <> bundles (bundleTable rows)
    <> (if Array.null timings then "" else "\n## Timings\n\n" <> mdTable (timingTable timings))
    <> failures rows
  where
  bundles t = if Array.null t.rows then "" else "\n## Bundles\n\n" <> mdTable t
  mdTable t =
    line t.headers <> line (map (const "---") t.headers) <> foldMap line t.rows
  line cells = "| " <> String.joinWith " | " (map (String.replaceAll (String.Pattern "|") (String.Replacement "\\|")) cells) <> " |\n"
  failures rs = case Array.filter (not <<< _.result.pass) rs of
    [] -> ""
    failed -> "\n## Failures\n\n" <> foldMap (\r -> "- " <> r.result.scenario <> "/" <> r.result.framework <> "/" <> r.result.variant <> ":\n" <> foldMap (\p -> "  ```\n" <> indent p <> "\n  ```\n") r.result.problems) failed
  indent = String.joinWith "\n" <<< map ("  " <> _) <<< String.split (String.Pattern "\n")

summaryLine :: Array ReportRow -> Array Timing -> String
summaryLine rows timings =
  show (Array.length (Array.filter _.result.pass rows)) <> " of " <> show (Array.length rows) <> " apps pass."
    <> case Array.mapMaybe _.within timings of
      [] -> ""
      checked -> " " <> show (Array.length (Array.filter identity checked)) <> " of " <> show (Array.length checked)
        <> " psx/ timings are within run-to-run noise of purs/."

html :: Array ReportRow -> Array Timing -> String
html rows timings =
  """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>PSX suite report</title>
<style>
  body { font: 14px/1.4 system-ui, sans-serif; margin: 2rem; color: #222; background: #fff; }
  table { border-collapse: collapse; margin: 1rem 0 2rem; }
  th, td { border: 1px solid #ccc; padding: 0.3rem 0.6rem; text-align: left; vertical-align: top; }
  th { background: #f4f4f4; }
  td.FAIL, td.NO { color: #b00; font-weight: 600; }
  @media (prefers-color-scheme: dark) {
    body { color: #ddd; background: #161616; }
    th { background: #262626; }
    th, td { border-color: #444; }
    td.FAIL, td.NO { color: #f77; }
  }
</style>
</head>
<body>
<h1>PSX suite report</h1>
<p>""" <> escape (summaryLine rows timings) <> "</p>\n"
    <> htmlTable (gridTable rows)
    <> (let t = bundleTable rows in if Array.null t.rows then "" else "<h2>Bundles</h2>\n" <> htmlTable t)
    <> (if Array.null timings then "" else "<h2>Timings</h2>\n" <> htmlTable (timingTable timings))
    <> "</body>\n</html>\n"
  where
  htmlTable t =
    "<table>\n<tr>" <> foldMap (\h -> "<th>" <> escape h <> "</th>") t.headers <> "</tr>\n"
      <> foldMap (\r -> "<tr>" <> foldMap (\c -> "<td class=\"" <> escape c <> "\">" <> escape c <> "</td>") r <> "</tr>\n") t.rows
      <> "</table>\n"
  escape =
    String.replaceAll (String.Pattern "\"") (String.Replacement "&quot;")
      <<< String.replaceAll (String.Pattern ">") (String.Replacement "&gt;")
      <<< String.replaceAll (String.Pattern "<") (String.Replacement "&lt;")
      <<< String.replaceAll (String.Pattern "&") (String.Replacement "&amp;")

spread :: Summary -> String
spread s = ms s.median <> " (" <> toStringWith (fixed 1) s.q1 <> "–" <> toStringWith (fixed 1) s.q3 <> ")"

ms :: Number -> String
ms n = toStringWith (fixed 1) n

signed :: Number -> String
signed n = (if n >= 0.0 then "+" else "") <> toStringWith (fixed 1) n

percent :: Number -> String
percent x = (if x >= 0.0 then "+" else "") <> toStringWith (fixed 0) (x * 100.0) <> "%"

seconds :: Number -> String
seconds = toStringWith (fixed 2)
