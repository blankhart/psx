-- | `spago run -p psx-suite` runs every scenario's apps in Chromium and checks
-- | them against the scenario's DOM contracts, then compares each framework's
-- | `psx/` and `purs/` bundles. `-- --accept` writes the contracts from the
-- | `purs/` apps instead of failing on them; further arguments name the
-- | scenarios to run. Run `spago build` first: the apps are bundled from the
-- | workspace's compiled output.
module Suite.Main (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (fixed, toStringWith)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Suite.Dom as Dom
import Suite.Driver (Browser, Page)
import Suite.Driver as Driver
import Suite.Scenario (Scenario, Step(..))
import Suite.Scenario.Counter (counter)
import Suite.Scenario.Todo (todo)

scenarios :: Array Scenario
scenarios = [ counter, todo ]

frameworks :: Array String
frameworks = [ "portico" ]

-- | `purs/` first, so that `--accept` writes the contracts from the control.
variants :: Array String
variants = [ "purs", "psx" ]

-- | One implementation of a scenario: the module
-- | `App.<Scenario>.<Framework>.<Variant>` under
-- | `test/suite/apps/<scenario>/<framework>/<variant>/`, whose
-- | `main :: Fixture -> Effect Unit` mounts the app on `#app`.
type App = { scenario :: Scenario, framework :: String, variant :: String }

moduleName :: App -> String
moduleName app =
  "App." <> capitalise app.scenario.name <> "." <> capitalise app.framework <> "." <> capitalise app.variant
  where
  capitalise s = String.toUpper (String.take 1 s) <> String.drop 1 s

label :: App -> String
label app = app.scenario.name <> "/" <> app.framework <> "/" <> app.variant

type Env =
  { root :: String
  , accept :: Boolean
  , browser :: Browser
  -- | The contracts `--accept` has written in this run.
  , written :: Ref (Set String)
  }

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  root <- Process.cwd
  chrome <- fromMaybe "" <$> Process.lookupEnv "CHROME_PATH"
  let
    accept = Array.elem "--accept" args
    names = Array.filter (_ /= "--accept") args
    chosen = Array.filter (\s -> Array.null names || Array.elem s.name names) scenarios
    unknown = Array.filter (\n -> not (Array.any (\s -> s.name == n) scenarios)) names
  if not (Array.null unknown) then do
    log $ "psx-suite: no scenario named " <> String.joinWith ", " unknown
    Process.setExitCode 2
  else do
    written <- Ref.new Set.empty
    launchAff_ do
      browser <- Driver.launch chrome
      let env = { root, accept, browser, written }
      failures <- for chosen \scenario -> for frameworks \framework -> do
        appFailures <- for variants \variant -> runApp env { scenario, framework, variant }
        bundleFailures <- compareBundles env scenario framework
        pure (sum appFailures + bundleFailures)
      Driver.close browser
      let total = sum (map sum failures)
      liftEffect do
        log $ if total == 0 then "psx-suite: all passed" else "psx-suite: " <> show total <> " failed"
        when (total > 0) (Process.setExitCode 1)

distDir :: String -> App -> String
distDir root app = Path.concat [ root, "test/suite/dist", app.scenario.name, app.framework, app.variant ]

-- | Bundles the app, opens it and runs the scenario's steps. The number of
-- | failures, each one logged.
runApp :: Env -> App -> Aff Int
runApp env app = do
  let
    dist = distDir env.root app
    module_ = Path.concat [ env.root, "output", moduleName app, "index.js" ]
  compiled <- liftEffect (FS.exists module_)
  if not compiled then
    fail [ moduleName app <> " has not been compiled; run `spago build` first" ]
  else do
    liftEffect do
      FS.mkdir' dist { recursive: true, mode: permsAll }
      FS.writeTextFile UTF8 (Path.concat [ dist, "index.html" ]) (page app)
    Driver.bundle { module: module_, outfile: Path.concat [ dist, "app.js" ] }
    result <- attempt do
      p <- Driver.open env.browser (Path.concat [ dist, "index.html" ])
      Driver.settle p
      problems <- Array.fold <$> for app.scenario.steps (runStep env app p)
      errors <- liftEffect (Driver.pageErrors p)
      Driver.closePage p
      pure (problems <> map ("page error: " <> _) errors)
    case result of
      Left error -> fail [ message error ]
      Right [] -> do
        liftEffect $ log $ "ok   " <> label app
        pure 0
      Right problems -> fail problems
  where
  fail problems = liftEffect do
    log $ "FAIL " <> label app
    for_ problems \problem -> log $ indent problem
    pure 1

-- | Runs one step, returning what went wrong.
runStep :: Env -> App -> Page -> Step -> Aff (Array String)
runStep env app p = case _ of
  Click testId -> do
    Driver.click p testId
    Driver.settle p
    pure []
  Type testId text -> do
    Driver.fill p testId text
    Driver.settle p
    pure []
  Expect name -> do
    actual <- Dom.print <<< Dom.normalise <$> Driver.snapshot p
    liftEffect (checkContract env app name actual)
  Measure name steps -> do
    start <- liftEffect Driver.now
    problems <- Array.fold <$> for steps (runStep env app p)
    end <- liftEffect Driver.now
    liftEffect $ log $ "     " <> label app <> " measure " <> show name <> ": "
      <> toStringWith (fixed 1) (end - start)
      <> " ms"
    pure problems

checkContract :: Env -> App -> String -> String -> Effect (Array String)
checkContract env app name actual = do
  let
    dir = Path.concat [ env.root, "test/suite/scenarios", app.scenario.name ]
    file = Path.concat [ dir, name <> ".html" ]
  exists <- FS.exists file
  expected <- if exists then Just <$> FS.readTextFile UTF8 file else pure Nothing
  written <- Set.member file <$> Ref.read env.written
  if env.accept && not written then do
    when (expected /= Just actual) do
      FS.mkdir' dir { recursive: true, mode: permsAll }
      FS.writeTextFile UTF8 file actual
      log $ "     wrote " <> Path.relative env.root file
    Ref.modify_ (Set.insert file) env.written
    pure []
  else case expected of
    Nothing -> pure [ "no contract " <> show name <> "; run with --accept to write it" ]
    Just e
      | e == actual -> pure []
      | otherwise -> pure [ "DOM differs from contract " <> show name <> ":\n" <> diff e actual ]

-- | The first line that differs, with the lines around it.
diff :: String -> String -> String
diff expected actual =
  let
    es = String.split (String.Pattern "\n") expected
    as = String.split (String.Pattern "\n") actual
    at = fromMaybe (Array.length es) $ Array.findIndex identity
      $ Array.zipWith (/=) (es <> [ "" ]) (as <> [ "" ])
    window xs = Array.slice (max 0 (at - 3)) (at + 4) xs
  in
    "expected:\n" <> String.joinWith "\n" (map ("  " <> _) (window es))
      <> "\nactual:\n"
      <> String.joinWith "\n" (map ("  " <> _) (window as))

-- | Reports each framework's `psx/` and `purs/` bundle sizes. The build plan
-- | asks that the two minified bundles differ only by the scope attributes
-- | PSX stamps, which cannot hold as stated: the `psx/` bundle also carries
-- | the backend's `Children` and `KeyedChildren` dispatch, `keyed` and
-- | `root`, and purs writes each module's name and line numbers into its
-- | pattern-match errors. So, as decided on psx#8, the comparison is
-- | reported and does not fail the run; bundle size is judged in step 13,
-- | against measured noise.
compareBundles :: Env -> Scenario -> String -> Aff Int
compareBundles env scenario framework = liftEffect do
  let
    app variant = { scenario, framework, variant }
    file variant = Path.concat [ distDir env.root (app variant), "app.js" ]
    name = scenario.name <> "/" <> framework
  present <- Array.all identity <$> for variants (FS.exists <<< file)
  when present do
    sizes <- for variants \variant -> do
      source <- FS.readTextFile UTF8 (file variant)
      gzip <- Driver.gzipSize (file variant)
      pure { variant, source, bytes: CodeUnits.length source, gzip }
    log $ "     " <> name <> " bundles: " <> String.joinWith ", "
      (map (\s -> s.variant <> " " <> show s.bytes <> " B (" <> show s.gzip <> " B gzip)") sizes)
    case sizes of
      [ purs, psx ]
        | purs.source == psx.source -> log $ "     " <> name <> " bundles are identical"
        | otherwise -> log $ "     " <> name <> " psx/ adds " <> show (psx.bytes - purs.bytes)
            <> " B ("
            <> show (psx.gzip - purs.gzip)
            <> " B gzip) to purs/"
      _ -> pure unit
  pure 0

page :: App -> String
page app =
  """<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>""" <> label app <> """</title>
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

