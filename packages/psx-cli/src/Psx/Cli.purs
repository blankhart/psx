-- | The psx command line.
-- |
-- | - `psx build [dir...]` lowers every `.psx` and `.psxt` file under the
-- |   directories (default `src`) to a `.purs` file beside it, writing only
-- |   files whose text changes. A file with styles also gets its stylesheet
-- |   (`Counter.css`) and companion module (`Counter/PsxStyle.purs` and
-- |   `.js`). It never overwrites a file psx did not write, and removes the
-- |   stylesheet and companion it wrote once a file has no styles. It also
-- |   deletes generated output whose source no longer exists.
-- | - `psx ignore [dir...] [--file .gitignore] [--check]` maintains exact
-- |   generated paths in a managed ignore block, or checks that it is current.
-- | - `psx lower <file> [-o <out.purs>] [--map <out.map.json>]` lowers one
-- |   file, to standard output unless `-o` is given.
-- | - `psx check [dir...] [--json] [-- <spago build arguments>]` builds, then
-- |   compiles with `spago build --json-errors` and reports the compiler's
-- |   errors and warnings at their `.psx` positions.
-- | - `psx fmt [--purs] [--check] [path...]` formats every `.psx` and `.psxt` file
-- |   at the paths (default `src`) in place. With `--check` it writes
-- |   nothing, and fails if a file is not formatted.
-- |
-- | - `psx gen-backend [mapping...]` writes each backend's generated module
-- |   and its conformance module from `psx-html-data` and the framework's
-- |   mapping (default: every `backends/mappings/*.json`).
-- | - `psx explain <element>` prints what `psx-html-data` knows about an
-- |   element: its attributes with their types and identifiers, and its
-- |   events.
-- |
-- | Compiling stays spago's job: a project runs `psx check`.
module Psx.Cli
  ( main
  , run
  ) where

import Prelude

import Data.Argonaut.Core (fromArray, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Psx.Cli.Check (Diagnostic, Severity(..), diagnosticJson, parseCompilerOutput, printDiagnostic, remap)
import Psx.Cli.Explain (explain)
import Psx.Cli.GenBackend (decodeMapping, generate)
import Psx.Cli.Process (runSync)
import Psx.Cst (PsxError)
import Psx.Format (formatPsxWith, formatPsxtWith)
import Psx.Format.Config (loadConfig)
import Effect.Exception as Exception
import Psx.Lower (Lowered, isGenerated, lower, lowerPsxt)
import Psx.Lower.Css as Css
import Psx.Lower.PositionMap (printJson)

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  status <- run args
  Process.setExitCode status

usage :: String
usage = String.joinWith "\n"
  [ "usage: psx build [dir...]"
  , "       psx lower <file.psx|file.psxt> [-o <out.purs>] [--map <out.map.json>]"
  , "       psx check [dir...] [--json] [-- <spago build arguments>]"
  , "       psx fmt [--purs] [--check] [path...]"
  , "       psx ignore [dir...] [--file .gitignore] [--check]"
  , "       psx gen-backend [mapping.json...]"
  , "       psx explain <element>"
  ]

-- | Runs a command and returns its exit status.
run :: Array String -> Effect Int
run args = case Array.uncons args of
  Just { head: "build", tail } -> build (dirs tail)
  Just { head: "lower", tail } -> lowerCommand tail
  Just { head: "ignore", tail } -> ignoreCommand tail
  Just { head: "check", tail } -> do
    let
      { before, after } = splitDashes tail
      json = Array.elem "--json" before
    check (dirs (Array.filter (_ /= "--json") before)) json after
  Just { head: "fmt", tail } -> do
    let roots = dirs (Array.filter (\arg -> arg /= "--check" && arg /= "--purs") tail)
    status <- fmt roots (Array.elem "--check" tail)
    pursStatus <- if Array.elem "--purs" tail then fmtPurs roots (Array.elem "--check" tail) else pure 0
    pure (max status pursStatus)
  Just { head: "gen-backend", tail } -> genBackend tail
  Just { head: "explain", tail: [ tag ] } -> case explain tag of
    Right text -> do
      log text
      pure 0
    Left message -> do
      error ("psx explain: " <> message)
      pure 1
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

-- | The generated file for a source: `Counter.psx` and `Counter.psxt` give
-- | `Counter.purs`.
generatedPath :: FilePath -> FilePath
generatedPath file = sibling file ".purs"

-- | A file beside the source with the same base name.
sibling :: FilePath -> String -> FilePath
sibling file ext = Path.concat [ Path.dirname file, Path.basenameWithoutExt file (Path.extname file) <> ext ]

-- | The companion module's files: `Counter.psx` gives `Counter/PsxStyle.purs`
-- | and `Counter/PsxStyle.js`.
companionPath :: FilePath -> String -> FilePath
companionPath file ext = Path.concat [ Path.dirname file, Path.basenameWithoutExt file (Path.extname file), "PsxStyle" <> ext ]

isSource :: FilePath -> Boolean
isSource path = Path.extname path == ".psx" || Path.extname path == ".psxt"

lowerFile :: FilePath -> Effect (Either (Array PsxError) Lowered)
lowerFile file = do
  text <- FS.readTextFile UTF8 file
  let names = { sourceName: Path.basename file, generatedName: Path.basename (generatedPath file) }
  pure if Path.extname file == ".psxt" then lowerPsxt names text else lower names text

printErrors :: FilePath -> Array PsxError -> Effect Unit
printErrors file errors = for_ errors \e ->
  error $ file <> ":" <> show (e.range.start.line + 1) <> ":" <> show (e.range.start.column + 1) <> ": error: "
    <> e.message

-- | Every `.psx` and `.psxt` file under the directories, skipping
-- | dependencies, build output and hidden directories.
findSources :: Array FilePath -> Effect (Array FilePath)
findSources = findFiles isSource

findFiles :: (FilePath -> Boolean) -> Array FilePath -> Effect (Array FilePath)
findFiles keepFile roots = Array.nub <<< Array.sort <<< Array.concat <$> traverse walk roots
  where
  walk path = do
    exists <- FS.exists path
    if not exists then pure []
    else do
      stats <- FS.lstat path
      if Stats.isSymbolicLink stats then pure []
      else if Stats.isDirectory stats then do
        names <- FS.readdir path
        let keep n = not (String.take 1 n == "." || Array.elem n [ "node_modules", "output", ".spago" ])
        Array.concat <$> for (Array.filter keep names) \n -> walk (Path.concat [ path, n ])
      else pure (if Stats.isFile stats && keepFile path then [ path ] else [])

-- | Lowers the sources under the directories and writes what changed.
-- | Returns the number of files that failed.
build :: Array FilePath -> Effect Int
build = buildReporting true

buildReporting :: Boolean -> Array FilePath -> Effect Int
buildReporting reportWarnings roots = do
  cleanOrphans roots
  sources <- findSources roots
  failures <- for sources \file -> do
    result <- lowerFile file
    case result of
      Left errors -> do
        printErrors file errors
        pure 1
      Right lowered -> do
        when reportWarnings (for_ lowered.warnings (error <<< printDiagnostic <<< attributeDiagnostic file))
        outputs <- generatedFiles file lowered
        -- Nothing is written if a file psx did not write is in the way.
        blocked <- Array.filterA (inTheWay file) outputs
        if Array.null blocked then do
          for_ outputs (writeGenerated file)
          removeEmptyCompanion file
          pure 0
        else pure 1
  let failed = Array.foldl (+) 0 failures
  pure (if failed > 0 then 1 else 0)

-- | A file psx writes for a source: its text, or `Nothing` if psx's copy
-- | should be removed.
type Output = { path :: FilePath, text :: Maybe String, marker :: String }

generatedFiles :: FilePath -> Lowered -> Effect (Array Output)
generatedFiles file lowered = do
  let
    purs = { path: generatedPath file, text: Just lowered.purs, marker: "-- Generated by psx from " }
    cssPath = sibling file ".css"
    companionPurs = companionPath file ".purs"
    companionJs = companionPath file ".js"
  case lowered.style of
    Nothing -> pure
      [ purs
      , { path: cssPath, text: Nothing, marker: "/* Generated by psx from " }
      , { path: companionPurs, text: Nothing, marker: "-- Generated by psx from " }
      , { path: companionJs, text: Nothing, marker: "// Generated by psx from " }
      ]
    Just style -> do
      cssImport <- importPath cssPath style.companion
      let names = { sourceName: Path.basename file, moduleName: style.moduleName, cssImport }
      pure
        [ purs
        , { path: cssPath, text: Just style.css, marker: "/* Generated by psx from " }
        , { path: companionPurs, text: Just style.companionPurs, marker: "-- Generated by psx from " }
        , { path: companionJs, text: Just (Css.companionForeign names style.scopes), marker: "// Generated by psx from " }
        ]

-- | Whether a file psx did not write is where it would write one; if so,
-- | says so.
inTheWay :: FilePath -> Output -> Effect Boolean
inTheWay source o = case o.text of
  Nothing -> pure false
  Just _ -> do
    exists <- FS.exists o.path
    current <- if exists then Just <$> FS.readTextFile UTF8 o.path else pure Nothing
    case current of
      Just text | String.take (String.length o.marker) text /= o.marker -> do
        error $ o.path <> ": error: not written by psx, so psx build leaves it alone; move it to lower " <> source
        pure true
      _ -> pure false

-- | Writes a generated file if it changed, or removes psx's copy of one
-- | that is no longer generated.
writeGenerated :: FilePath -> Output -> Effect Unit
writeGenerated source o = do
  exists <- FS.exists o.path
  current <- if exists then Just <$> FS.readTextFile UTF8 o.path else pure Nothing
  case current, o.text of
    Just text, Nothing
      | String.take (String.length o.marker) text == o.marker -> do
          FS.unlink o.path
          log $ "removed " <> o.path
    _, Just text
      | current /= Just text -> do
          FS.mkdir' (Path.dirname o.path) { recursive: true, mode: permsAll }
          FS.writeTextFile UTF8 o.path text
          log $ "wrote " <> o.path <> " from " <> source
    _, _ -> pure unit

-- | Only files with a complete generated header and a standard output name
-- | belong to psx. A source name is a basename, never a path from the header.
cleanOrphans :: Array FilePath -> Effect Unit
cleanOrphans roots = do
  absoluteRoots <- traverse (Path.resolve []) roots
  files <- findFiles (\p -> Array.elem (Path.extname p) [ ".purs", ".css", ".js" ]) absoluteRoots
  for_ files \file -> do
    text <- FS.readTextFile UTF8 file
    let
      firstLine = String.takeWhile (_ /= String.codePointFromChar '\n') text
      line = fromMaybe firstLine (String.stripSuffix (String.Pattern "\r") firstLine)
      named = case Path.extname file of
        ".css" -> String.stripPrefix (String.Pattern "/* Generated by psx from ") line >>= String.stripSuffix (String.Pattern " */")
        ".js" -> String.stripPrefix (String.Pattern "// Generated by psx from ") line
        _ -> String.stripPrefix (String.Pattern "-- Generated by psx from ") line
    case named of
      Just name | isSource name && Path.basename name == name && not (String.contains (String.Pattern "\\") name) -> do
        let
          localSource = Path.concat [ Path.dirname file, name ]
          parentSource = Path.concat [ Path.dirname (Path.dirname file), name ]
          localOutput = Path.extname file /= ".js" && file == sibling localSource (Path.extname file)
          companion = Array.elem (Path.extname file) [ ".purs", ".js" ]
            && file == companionPath parentSource (Path.extname file)
        -- A source named PsxStyle can give either a sibling or a companion
        -- named PsxStyle.purs. Keep the file if either source still exists.
        exists <- Array.any identity <$> traverse FS.exists
          ((if localOutput then [ localSource ] else []) <> (if companion then [ parentSource ] else []))
        when (not exists && (localOutput || companion)) do
          FS.unlink file
          log ("removed " <> file)
          when companion (removeEmptyCompanion parentSource)
      _ -> pure unit

removeEmptyCompanion :: FilePath -> Effect Unit
removeEmptyCompanion source = do
  let dir = Path.dirname (companionPath source ".purs")
  exists <- FS.exists dir
  when exists do
    stats <- FS.lstat dir
    when (Stats.isDirectory stats && not (Stats.isSymbolicLink stats)) do
      names <- FS.readdir dir
      when (Array.null names) (FS.rmdir dir)

-- Ignore lists -----------------------------------------------------------------

ignoreStart :: String
ignoreStart = "# BEGIN psx generated files"

ignoreEnd :: String
ignoreEnd = "# END psx generated files"

ignoreCommand :: Array String -> Effect Int
ignoreCommand = parse [] ".gitignore" false
  where
  parse roots file checkOnly args = case Array.uncons args of
    Nothing -> ignoreFiles (if Array.null roots then [ "src" ] else roots) file checkOnly
    Just { head: "--check", tail } -> parse roots file true tail
    Just { head: "--file", tail } -> case Array.uncons tail of
      Just { head: name, tail: rest } | String.take 1 name /= "-" -> parse roots name checkOnly rest
      _ -> invalid
    Just { head, tail } | String.take 1 head /= "-" -> parse (Array.snoc roots head) file checkOnly tail
    _ -> invalid
  invalid = error usage *> pure 2

ignoreFiles :: Array FilePath -> FilePath -> Boolean -> Effect Int
ignoreFiles roots file checkOnly = do
  sources <- findSources roots
  absoluteFile <- Path.resolve [] file
  paths <- Array.concat <$> for sources
    ( \source -> do
        absolute <- Path.resolve [] source
        pure
          ( map (Path.relative (Path.dirname absoluteFile))
              [ generatedPath absolute, sibling absolute ".css", companionPath absolute ".purs", companionPath absolute ".js" ]
          )
    )
  if Array.any (\p -> p == ".." || String.take 3 p == ".." <> Path.sep || Path.isAbsolute p) paths then do
    error "psx ignore: all generated paths must be below the ignore file's directory"
    pure 1
  else if Array.any (\p -> String.contains (String.Pattern "\n") p || String.contains (String.Pattern "\r") p) paths then do
    error "psx ignore: paths containing newlines cannot be represented in an ignore file"
    pure 1
  else do
    exists <- FS.exists file
    current <- if exists then FS.readTextFile UTF8 file else pure ""
    let
      -- Anchor every literal path to the ignore file. Reserve style paths even
      -- when a source currently has no styles, so style edits need no refresh.
      block = String.joinWith "\n" ([ ignoreStart ] <> map (("/" <> _) <<< escapeIgnore) (Array.nub (Array.sort paths)) <> [ ignoreEnd ])
    case replaceIgnoreBlock current block of
      Left message -> error ("psx ignore: " <> message) *> pure 1
      Right updated
        | updated == current -> pure 0
        | checkOnly -> error (file <> ": stale psx ignore list (run psx ignore)") *> pure 1
        | otherwise -> FS.writeTextFile UTF8 file updated *> log ("updated " <> file) *> pure 0

escapeIgnore :: String -> String
escapeIgnore path = Array.foldl
  (\text char -> String.replaceAll (String.Pattern char) (String.Replacement ("\\" <> char)) text)
  (String.replaceAll (String.Pattern Path.sep) (String.Replacement "/") path)
  [ "\\", "*", "?", "[", "]", " ", "!", "#" ]

-- Preserve everything outside the block, including final-newline policy.
-- Refuse ambiguous markers instead of risking a user's rules.
replaceIgnoreBlock :: String -> String -> Either String String
replaceIgnoreBlock current block = do
  let
    ls = String.split (String.Pattern "\n") current
    marker line = fromMaybe line (String.stripSuffix (String.Pattern "\r") line)
    indices name = Array.catMaybes (Array.mapWithIndex (\i line -> if marker line == name then Just i else Nothing) ls)
    starts = indices ignoreStart
    ends = indices ignoreEnd
    crlf = String.contains (String.Pattern "\r\n") current
    newline = if crlf then "\r\n" else "\n"
    formatted = String.replaceAll (String.Pattern "\n") (String.Replacement newline) block
  case starts, ends of
    [], [] -> Right (current <> (if current == "" || Array.last ls == Just "" then "" else newline) <> formatted <> newline)
    [ start ], [ end ] | start < end -> Right (String.joinWith "\n" (Array.take start ls <> [ formatted <> (if crlf && end < Array.length ls - 1 then "\r" else "") ] <> Array.drop (end + 1) ls))
    _, _ -> Left "malformed or duplicate psx ignore block"

-- | The stylesheet's path as the companion's compiled foreign file imports
-- | it: from `output/<companion>/foreign.js` in the spago workspace that
-- | holds the source.
importPath :: FilePath -> String -> Effect String
importPath cssPath companion = do
  cwd <- Process.cwd
  absolute <- Path.resolve [ cwd ] cssPath
  root <- workspaceRoot (Path.dirname absolute)
  let
    from = Path.concat [ root, "output", companion ]
    relative = Path.relative from absolute
  pure (String.replaceAll (String.Pattern "\\") (String.Replacement "/") relative)

-- | The nearest directory at or above this one whose `spago.yaml` declares
-- | a workspace, or the current directory.
workspaceRoot :: FilePath -> Effect FilePath
workspaceRoot dir = do
  let config = Path.concat [ dir, "spago.yaml" ]
  exists <- FS.exists config
  isWorkspace <-
    if exists then String.contains (String.Pattern "\nworkspace:") <<< ("\n" <> _) <$> FS.readTextFile UTF8 config
    else pure false
  if isWorkspace then pure dir
  else if Path.dirname dir == dir then Process.cwd
  else workspaceRoot (Path.dirname dir)

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
  built <- buildReporting (not json) roots
  if built /= 0 then pure built
  else do
    spago <- fromMaybe "npx spago" <$> Process.lookupEnv "PSX_SPAGO"
    let
      command = String.split (String.Pattern " ") spago
      exe = fromMaybe "npx" (Array.head command)
    result <- runSync exe (Array.drop 1 command <> [ "build", "--json-errors" ] <> spagoArgs)
    let diagnostics = parseCompilerOutput result.stdout <> parseCompilerOutput result.stderr
    remapped <- traverse remapDiagnostic diagnostics
    sources <- if json then findSources roots else pure []
    attributeWarnings <- Array.concat <$> for sources
      ( \file -> do
          loweredFile <- lowerFile file
          pure case loweredFile of
            Right lowered -> map (attributeDiagnostic file) lowered.warnings
            Left _ -> []
      )
    if json then log (stringify (fromArray (map diagnosticJson (attributeWarnings <> remapped))))
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

-- Formatting ---------------------------------------------------------------------

-- | Formats the sources at the paths in place, or with `check` reports those
-- | that are not formatted. Fails if any file has errors or, with `check`,
-- | is not formatted.
fmt :: Array FilePath -> Boolean -> Effect Int
fmt roots checkOnly = do
  sources <- findSources roots
  failures <- for sources \file -> do
    text <- FS.readTextFile UTF8 file
    loaded <- Exception.try (loadConfig file)
    case loaded of
      Left err -> do
        error (Exception.message err)
        pure 1
      Right config -> do
        case (if Path.extname file == ".psxt" then formatPsxtWith config else formatPsxWith config) text of
          Left errors -> do
            printErrors file errors
            pure 1
          Right formatted
            | formatted == text -> pure 0
            | checkOnly -> do
                error $ file <> ": not formatted (run psx fmt)"
                pure 1
            | otherwise -> do
                FS.writeTextFile UTF8 file formatted
                log $ "formatted " <> file
                pure 0
  pure (if Array.foldl (+) 0 failures > 0 then 1 else 0)

-- Generating backends -------------------------------------------------------------

-- | Writes each mapping's backend and conformance modules, if they changed.
genBackend :: Array FilePath -> Effect Int
genBackend args = do
  mappings <-
    if Array.null args then do
      let dir = "backends/mappings"
      names <- FS.readdir dir
      pure (map (\n -> Path.concat [ dir, n ]) (Array.sort (Array.filter (\n -> Path.extname n == ".json") names)))
    else pure args
  failures <- for mappings \file -> do
    text <- FS.readTextFile UTF8 file
    case jsonParser text >>= decodeMapping >>= \m -> map (Tuple m) (generate file m) of
      Left message -> do
        error (file <> ": error: " <> message)
        pure 1
      Right (Tuple m generated) -> do
        let
          path = Path.concat ([ m.directory ] <> String.split (String.Pattern ".") m.module)
          output p t = { path: p, text: Just t, marker: "-- Generated by psx gen-backend from " }
        for_ [ output (path <> ".purs") generated.backend, output (Path.concat [ path, "Conformance.purs" ]) generated.conformance ]
          (writeGenerated file)
        pure 0
  pure (if Array.foldl (+) 0 failures > 0 then 1 else 0)

-- purs-tidy receives explicit handwritten files, never generated modules.
fmtPurs :: Array FilePath -> Boolean -> Effect Int
fmtPurs roots checkOnly = do
  files <- findFiles (\path -> Path.extname path == ".purs") roots
  handwritten <- Array.filterA (\file -> not <<< isGenerated <$> FS.readTextFile UTF8 file) files
  if Array.null handwritten then pure 0
  else do
    result <- runSync "purs-tidy" ([ if checkOnly then "check" else "format-in-place" ] <> handwritten)
    unless (result.stdout == "") (log result.stdout)
    unless (result.stderr == "") (error result.stderr)
    pure result.status

attributeDiagnostic :: FilePath -> PsxError -> Diagnostic
attributeDiagnostic file warning =
  { severity: Warning, code: "UnknownAttribute", message: warning.message, file: Just file, range: Just warning.range }
