-- | `spago run -p psx-suite` runs every scenario's apps in Chromium and checks
-- | them against the scenario's DOM contracts, then writes the report (see
-- | `Suite.Report`) to `test/suite/dist/report/`. Run `spago build` first:
-- | the apps are bundled from the workspace's compiled output. Each app is
-- | also compiled with purs-backend-es and bundled from that, for the
-- | report's bundle sizes only; those bundles are not run.
-- |
-- | Arguments, before the names of the scenarios to run (all by default):
-- |
-- | - `--accept` writes the contracts from the `purs/` apps instead of
-- |   failing on them.
-- | - `--runs N` runs every `Measure` N times instead of the scenario's own
-- |   count.
-- | - `--control` also runs each framework's `purs/` app a second time, so
-- |   the report can measure run-to-run noise, and checks that `psx/` is
-- |   within it on every measure; the run fails if it is not.
-- | - `--build-time` times `psx build` and `spago build` for every app.
-- | - `--history FILE` compares the timings with the last run recorded in
-- |   FILE, a JSON Lines file, and writes it with this run added to the
-- |   report directory.
module Suite.Main (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (fixed, toStringWith)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for, traverse)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Suite.Contract as Contract
import Suite.Dom as Dom
import Suite.Driver (Browser, Page, TimedStep(..))
import Suite.Driver as Driver
import Suite.Report (AppResult)
import Suite.Report as Report
import Suite.Scenario (Measure, Scenario, Step(..))
import Suite.Scenario.Counter (counter)
import Suite.Scenario.Nested (nested)
import Suite.Scenario.Svg (svg)
import Suite.Scenario.Table (table)
import Suite.Scenario.Todo (todo)
import Suite.Scenario.Typeahead (typeahead)
import Suite.Stats (summarise)

scenarios :: Array Scenario
scenarios = [ counter, todo, typeahead, nested, svg, table ]

-- | The frameworks a scenario has apps for: the directories under
-- | `test/suite/apps/<scenario>/`.
frameworks :: String -> Scenario -> Effect (Array String)
frameworks root scenario = do
  let dir = Path.concat [ root, "test/suite/apps", scenario.name ]
  exists <- FS.exists dir
  if exists then Array.sort <$> FS.readdir dir else pure []

-- | `purs/` first, so that `--accept` writes the contracts from the control.
variants :: Array String
variants = [ "purs", "psx" ]

-- | One implementation of a scenario: the module
-- | `App.<Scenario>.<Framework>.<Variant>` under
-- | `test/suite/apps/<scenario>/<framework>/<variant>/`, whose
-- | `main :: Fixture -> Effect Unit` mounts the app on `#app`. The control
-- | is a second run of the `purs/` app.
type App = { scenario :: Scenario, framework :: String, variant :: String, control :: Boolean }

moduleName :: App -> String
moduleName app =
  "App." <> capitalise app.scenario.name <> "." <> capitalise app.framework <> "." <> capitalise app.variant
  where
  capitalise s = String.toUpper (String.take 1 s) <> String.drop 1 s

label :: App -> String
label app = app.scenario.name <> "/" <> app.framework <> "/" <> app.variant <> if app.control then " (control)" else ""

type Options =
  { accept :: Boolean
  , runs :: Maybe Int
  , control :: Boolean
  , buildTime :: Boolean
  , history :: Maybe String
  , names :: Array String
  }

parseOptions :: Array String -> Either String Options
parseOptions = go { accept: false, runs: Nothing, control: false, buildTime: false, history: Nothing, names: [] }
  where
  go options args = case Array.uncons args of
    Nothing -> Right options
    Just { head: "--accept", tail } -> go options { accept = true } tail
    Just { head: "--control", tail } -> go options { control = true } tail
    Just { head: "--build-time", tail } -> go options { buildTime = true } tail
    Just { head: "--runs", tail } -> case Array.uncons tail >>= \t -> { n: _, rest: t.tail } <$> Int.fromString t.head of
      Just { n, rest } | n > 0 -> go options { runs = Just n } rest
      _ -> Left "--runs takes a positive number"
    Just { head: "--history", tail } -> case Array.uncons tail of
      Just t -> go options { history = Just t.head } t.tail
      Nothing -> Left "--history takes a file"
    Just { head, tail }
      | String.take 2 head == "--" -> Left ("unknown option " <> head)
      | otherwise -> go options { names = Array.snoc options.names head } tail

type Env =
  { root :: String
  , contracts :: String
  , options :: Options
  , browser :: Browser
  -- | The contracts `--accept` has written in this run.
  , written :: Ref (Set String)
  -- | Whether purs-backend-es compiled the workspace to `output-es/`.
  , es :: Boolean
  }

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  root <- Process.cwd
  contracts <- fromMaybe root <$> Process.lookupEnv "PSX_SUITE_CONTRACT_ROOT"
  chrome <- fromMaybe "" <$> Process.lookupEnv "CHROME_PATH"
  case parseOptions args of
    Left problem -> do
      log $ "psx-suite: " <> problem
      Process.setExitCode 2
    Right options -> do
      let
        chosen = Array.filter (\s -> Array.null options.names || Array.elem s.name options.names) scenarios
        unknown = Array.filter (\n -> not (Array.any (\s -> s.name == n) scenarios)) options.names
      if not (Array.null unknown) then do
        log $ "psx-suite: no scenario named " <> String.joinWith ", " unknown
        Process.setExitCode 2
      else do
        written <- Ref.new Set.empty
        built <- Driver.buildEs root
        unless built.ok do
          log "psx-suite: purs-backend-es failed, so the report has no purs-backend-es bundle sizes:"
          log (indent built.output)
        launchAff_ do
          browser <- Driver.launch chrome
          let env = { root, contracts, options, browser, written, es: built.ok }
          results <- Array.concat <<< Array.concat <$> for chosen \scenario ->
            liftEffect (frameworks root scenario) >>= traverse (runFramework env scenario)
          Driver.close browser
          liftEffect do
            report <- Report.write
              { root
              , out: Path.concat [ root, "test/suite/dist/report" ]
              , buildTime: options.buildTime
              , history: options.history
              }
              results
            let
              failed = Array.length (Array.filter (\r -> not r.pass) results)
              noisy = if options.control then report.outsideNoise else []
            for_ noisy \line -> log $ "     outside noise: " <> line
            log $ "psx-suite: " <> (if failed == 0 then "all passed" else show failed <> " failed")
              <> (if Array.null noisy then "" else "; " <> show (Array.length noisy) <> " psx/ timings outside run-to-run noise")
              <> "; report in "
              <> Path.relative root (Path.concat [ root, "test/suite/dist/report" ])
            when (failed > 0 || not (Array.null noisy)) (Process.setExitCode 1)

distDir :: String -> App -> String
distDir root app = Path.concat [ root, "test/suite/dist", app.scenario.name, app.framework, app.variant ]

-- | An app while its steps run.
type Running =
  { app :: App
  , page :: Page
  , problems :: Ref (Array String)
  , measures :: Ref (Array { name :: String, samples :: Array Number })
  -- | Set once a step has thrown: the app runs no further steps.
  , broken :: Ref Boolean
  }

-- | Runs a framework's apps for the scenario side by side, one page each,
-- | doing every step in each app before the next step, so their timings
-- | interleave and drift on the runner affects them alike.
runFramework :: Env -> Scenario -> String -> Aff (Array AppResult)
runFramework env scenario framework = do
  let
    apps =
      map (\variant -> { scenario, framework, variant, control: false }) variants
        <> (if env.options.control then [ { scenario, framework, variant: "purs", control: true } ] else [])
  prepared <- for apps (prepare env)
  running <- for prepared case _ of
    Left problem -> pure (Left problem)
    Right p -> do
      opened <- attempt do
        page <- Driver.open env.browser (Path.concat [ distDir env.root p.app, pageFile p.app ])
        Driver.settle page
        pure page
      case opened of
        Left error -> pure (Left { app: p.app, bundle: Just p.bundle, esBundle: p.esBundle, problem: message error })
        Right page -> liftEffect do
          problems <- Ref.new []
          measures <- Ref.new []
          broken <- Ref.new false
          pure (Right { running: { app: p.app, page, problems, measures, broken }, bundle: p.bundle, esBundle: p.esBundle })
  let live = Array.mapMaybe (case _ of
        Right r -> Just r.running
        Left _ -> Nothing) running
  for_ scenario.steps \step -> runStep env live step
  results <- for running case _ of
    Left failure -> do
      liftEffect $ report failure.app [ failure.problem ]
      pure (result failure.app false [ failure.problem ] [] failure.bundle failure.esBundle)
    Right { running: r, bundle, esBundle } -> do
      errors <- liftEffect (Driver.pageErrors r.page)
      Driver.closePage r.page
      liftEffect do
        problems <- (_ <> map ("page error: " <> _) errors) <$> Ref.read r.problems
        measures <- Ref.read r.measures
        report r.app problems
        pure (result r.app (Array.null problems) problems measures (Just bundle) esBundle)
  liftEffect (compareBundles scenario framework results)
  pure results
  where
  result app pass problems measures bundle esBundle =
    { scenario: scenario.name, framework, variant: app.variant, control: app.control, pass, problems, measures, bundle, esBundle }
  report app problems = do
    log $ (if Array.null problems then "ok   " else "FAIL ") <> label app
    for_ problems \problem -> log $ indent problem

type Size = { bytes :: Int, gzip :: Int }

-- | Bundles the app and writes the page that loads it, and bundles it again
-- | from purs-backend-es's output to measure it.
prepare
  :: Env
  -> App
  -> Aff (Either { app :: App, bundle :: Maybe Size, esBundle :: Maybe Size, problem :: String } { app :: App, bundle :: Size, esBundle :: Maybe Size })
prepare env app = do
  let
    dist = distDir env.root app
    module_ = Path.concat [ env.root, "output", moduleName app, "index.js" ]
    js = Path.concat [ dist, "app.js" ]
  compiled <- liftEffect (FS.exists module_)
  if not compiled then
    pure (Left { app, bundle: Nothing, esBundle: Nothing, problem: moduleName app <> " has not been compiled; run `spago build` first" })
  else do
    let css = Path.concat [ dist, "app.css" ]
    -- The control loads the bundle the `purs/` app has just written.
    unless app.control do
      liftEffect do
        FS.mkdir' dist { recursive: true, mode: permsAll }
        stale <- FS.exists css
        when stale (FS.unlink css)
      -- esbuild writes app.css beside app.js when the app imports CSS, as a
      -- PSX module with scoped styles does through its companion module.
      Driver.bundle { module: module_, outfile: js }
    esBundle <- if app.control || not env.es then pure Nothing else bundleEs env app dist
    liftEffect do
      styled <- FS.exists css
      FS.writeTextFile UTF8 (Path.concat [ dist, pageFile app ]) (pageHtml app styled)
      bundle <- sizeOf js
      pure (Right { app, bundle, esBundle })

-- | The app bundled from `output-es/`, with the same esbuild settings, or
-- | nothing if it cannot be: the size is reported and the bundle is not run.
bundleEs :: Env -> App -> String -> Aff (Maybe Size)
bundleEs env app dist = do
  let
    module_ = Path.concat [ env.root, "output-es", moduleName app, "index.js" ]
    js = Path.concat [ dist, "es", "app.js" ]
  liftEffect $ FS.mkdir' (Path.dirname js) { recursive: true, mode: permsAll }
  bundled <- attempt (Driver.bundle { module: module_, outfile: js })
  case bundled of
    Left error -> liftEffect do
      log $ "     " <> label app <> ": no purs-backend-es bundle: " <> message error
      pure Nothing
    Right _ -> liftEffect (Just <$> sizeOf js)

sizeOf :: String -> Effect Size
sizeOf js = do
  source <- FS.readTextFile UTF8 js
  gzip <- Driver.gzipSize js
  pure { bytes: CodeUnits.length source, gzip }

-- | Runs one step in every app that has not broken.
runStep :: Env -> Array Running -> Step -> Aff Unit
runStep env live = case _ of
  Measure m -> do
    let runs = fromMaybe m.runs env.options.runs
    -- Each run starts with the next app, so none is always first.
    for_ (Array.range 0 (runs - 1)) \run -> for_ (rotate run live) \r -> guarded r do
      for_ m.setup (untimed env r)
      times <- for m.steps (timed env r)
      liftEffect $ Ref.modify_ (addSample m (sum times)) r.measures
    liftEffect $ for_ live \r -> do
      samples <- Array.find (\x -> x.name == m.name) <$> Ref.read r.measures
      for_ samples \s -> do
        let t = summarise s.samples
        log $ "     " <> label r.app <> " measure " <> show m.name <> ": median "
          <> ms t.median
          <> " (quartiles "
          <> ms t.q1
          <> " to "
          <> ms t.q3
          <> ", "
          <> show t.n
          <> " runs)"
  step -> for_ live \r -> guarded r (untimed env r step)
  where
  ms n = toStringWith (fixed 1) n <> " ms"

rotate :: forall a. Int -> Array a -> Array a
rotate n xs = Array.drop k xs <> Array.take k xs
  where
  k = if Array.null xs then 0 else n `mod` Array.length xs

addSample :: Measure -> Number -> Array { name :: String, samples :: Array Number } -> Array { name :: String, samples :: Array Number }
addSample m t measures = case Array.findIndex (\x -> x.name == m.name) measures of
  Just i -> fromMaybe measures (Array.modifyAt i (\x -> x { samples = Array.snoc x.samples t }) measures)
  Nothing -> Array.snoc measures { name: m.name, samples: [ t ] }

-- | Runs the action unless the app has broken; an exception breaks it.
guarded :: Running -> Aff Unit -> Aff Unit
guarded r action = do
  broken <- liftEffect (Ref.read r.broken)
  unless broken do
    outcome <- attempt action
    case outcome of
      Left error -> liftEffect do
        Ref.write true r.broken
        addProblem r (message error)
      Right _ -> pure unit

addProblem :: Running -> String -> Effect Unit
addProblem r p = Ref.modify_ (_ <> [ p ]) r.problems

untimed :: Env -> Running -> Step -> Aff Unit
untimed env r = case _ of
  Click testId -> do
    Driver.click r.page testId
    Driver.settle r.page
  ClickNth prefix index -> do
    Driver.clickNth r.page prefix index
    Driver.settle r.page
  Type testId text -> do
    Driver.fill r.page testId text
    Driver.settle r.page
  Press testId key -> do
    Driver.press r.page testId key
    Driver.settle r.page
  Expect name -> do
    actual <- snapshot r
    liftEffect (checkContract env r name actual)
  Await name -> do
    expected <- liftEffect (readContract env r.app name)
    let
      -- Polls for the contract; for one `--accept` will write, waits
      -- instead for the DOM to stop changing.
      poll waited previous = do
        actual <- snapshot r
        let
          matches = case expected of
            Just e -> e == Contract.committed actual
            Nothing -> false
          stable = previous == Just actual && waited >= 1000
        if matches || waited >= 5000 || (env.options.accept && not r.app.control && r.app.variant == "purs" && stable) then
          liftEffect (checkContract env r name actual)
        else do
          delay (Milliseconds 25.0)
          poll (waited + 25) (Just actual)
    poll 0 Nothing
  -- A measure inside a measure is run once, untimed.
  Measure m -> do
    for_ m.setup (untimed env r)
    for_ m.steps (untimed env r)

-- | The time of a step that can be timed; any other is run untimed.
timed :: Env -> Running -> Step -> Aff Number
timed env r = case _ of
  Click testId -> Driver.timed r.page (TimedClick testId)
  ClickNth prefix index -> Driver.timed r.page (TimedClickNth prefix index)
  Type testId text -> Driver.timed r.page (TimedType testId text)
  Press testId key -> Driver.timed r.page (TimedPress testId key)
  step -> untimed env r step $> 0.0

snapshot :: Running -> Aff String
snapshot r = Dom.print <<< Dom.normalise <$> Driver.snapshot r.page

contractFile :: Env -> App -> String -> String
contractFile env app name = Path.concat [ env.contracts, "test/suite/scenarios", app.scenario.name, name <> ".html" ]

readContract :: Env -> App -> String -> Effect (Maybe String)
readContract env app name = do
  let file = contractFile env app name
  exists <- FS.exists file
  if exists then Just <$> FS.readTextFile UTF8 file else pure Nothing

checkContract :: Env -> Running -> String -> String -> Effect Unit
checkContract env r name printed = do
  let
    file = contractFile env r.app name
    actual = Contract.committed printed
  expected <- readContract env r.app name
  written <- Set.member file <$> Ref.read env.written
  if env.options.accept && not written && not r.app.control then do
    when (expected /= Just actual) do
      FS.mkdir' (Path.dirname file) { recursive: true, mode: permsAll }
      FS.writeTextFile UTF8 file actual
      log $ "     wrote " <> Path.relative env.root file
    Ref.modify_ (Set.insert file) env.written
  else case expected of
    Nothing -> addProblem r $ "no contract " <> show name <> "; run with --accept to write it"
    Just e
      | e == actual -> pure unit
      | otherwise -> do
          -- The whole DOM, for a contract that keeps only its head.
          let saved = Path.concat [ distDir env.root r.app, name <> ".actual.html" ]
          FS.writeTextFile UTF8 saved printed
          addProblem r $ "DOM differs from contract " <> show name <> " (all of it is in "
            <> Path.relative env.root saved
            <> "):\n"
            <> Contract.diff e actual

-- | Logs each framework's `psx/` and `purs/` bundle sizes, from purs's own
-- | code generator and from purs-backend-es. The build plan asks that the
-- | two minified bundles differ only by the scope attributes PSX stamps,
-- | which cannot hold as stated: the `psx/` bundle also carries the
-- | backend's `Children` and `Content` dispatch, `keyed` and `root`, and
-- | purs writes each module's name and line numbers into its pattern-match
-- | errors. So, as decided on psx#8, the comparison is reported and does not
-- | fail the run. purs-backend-es inlines dictionaries whose instances are
-- | known, so its gap is what a general optimiser leaves of that dispatch
-- | (psx#34).
compareBundles :: Scenario -> String -> Array AppResult -> Effect Unit
compareBundles scenario framework results = do
  let
    name = scenario.name <> "/" <> framework
    find variant = Array.find (\r -> r.variant == variant && not r.control) results
    logSizes codegen purs psx = do
      log $ "     " <> name <> " bundles (" <> codegen <> "): purs " <> show purs.bytes <> " B (" <> show purs.gzip <> " B gzip), psx "
        <> show psx.bytes
        <> " B ("
        <> show psx.gzip
        <> " B gzip)"
      log $ "     " <> name <> " psx/ adds " <> show (psx.bytes - purs.bytes) <> " B (" <> show (psx.gzip - purs.gzip)
        <> " B gzip) to purs/ ("
        <> codegen
        <> ")"
  case find "purs" >>= _.bundle, find "psx" >>= _.bundle of
    Just purs, Just psx -> logSizes "purs" purs psx
    _, _ -> pure unit
  case find "purs" >>= _.esBundle, find "psx" >>= _.esBundle of
    Just purs, Just psx -> logSizes "purs-backend-es" purs psx
    _, _ -> pure unit

pageFile :: App -> String
pageFile app = if app.control then "control.html" else "index.html"

pageHtml :: App -> Boolean -> String
pageHtml app styled =
  """<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>""" <> label app <> """</title>""" <> (if styled then "\n    <link rel=\"stylesheet\" href=\"app.css\">" else "") <> """
  </head>
  <body>
    <div id="root"><div id="app"></div></div>
    <script>window.psxFixture = """ <> app.scenario.fixture <> """;</script>
    <script src="app.js"></script>
  </body>
</html>
"""

indent :: String -> String
indent = String.joinWith "\n" <<< map ("     " <> _) <<< String.split (String.Pattern "\n")
