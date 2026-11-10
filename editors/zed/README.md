# PSX in Zed (local development extension)

PSX (`.psx`) and PSXT (`.psxt`) are separate registered languages. Both use the
pinned grammars from #66. PSX highlights PureScript and markup in one tree;
PSXT injects **PSX** into its PureScript script, so nested markup still works.
Style bodies inject Zed's built-in CSS language. No PureScript extension is
needed for highlighting. The PureScript grammar's MIT notice remains in the
pinned source repository at `editors/tree-sitter-psx/vendor/purescript/LICENSE`.

## Highlighting installation

1. Clone this repository. Open Zed's Extensions view (`zed: extensions`).
2. Choose **Install Dev Extension** and select this checkout's **editors/zed**
   directory, not the repository root. Zed fetches the exact grammar commits and
   compiles both grammars with its WASI SDK. Network access to GitHub is needed
   on first installation. No extension-store publication is involved.
3. Open `test/fixtures/example.psx` and `test/fixtures/example.psxt` below this
   directory. The status bar must identify PSX and PSXT respectively. Check
   PureScript keywords, tags, holes, template heads and CSS colours. Bracket
   matching, two-space indentation and context-sensitive comments are provided.
4. With `"enable_language_server": false` for both languages in Zed settings,
   repeat these checks: highlighting must work independently of a server.

```json
{
  "languages": {
    "PSX": { "enable_language_server": false },
    "PSXT": { "enable_language_server": false }
  }
}
```

The grammar revisions are full commit IDs, never branches. The language query
files are copies of the corresponding shared queries; the CI gate rejects drift.
After editing queries, copy them from `editors/tree-sitter-psx` and reinstall the
development extension. To try unpublished grammar edits, temporarily change
`repository` in both manifest grammar entries to `file:///absolute/path/to/psx`
and `rev` to the local committed revision. Keep the `path` entries unchanged;
restore reproducible public pins before committing.

## Language server prerequisites and contract

Build the existing server in the PSX checkout with Node 22, purs 0.15.15 and
Spago 1.0.5:

```sh
npm ci
npx spago build
```

The shared stdio launch is:

```sh
node /absolute/path/to/psx/packages/psx-language-server/bin/psx-language-server.mjs --stdio
```

See [the shared server contract](../../packages/psx-language-server/README.md).
The working directory is not configuration: LSP `rootUri`/workspace folders
select the consumer workspace. Optional `projectRoot` explicitly overrides it.
Initialization options also accept `purescriptLanguageServer` and `purescript`
settings (including `pursExe` and `outputDirectory`). Build the consumer's lowered
sources and dependencies before compiler features:

```sh
node /absolute/path/to/psx/packages/psx-cli/bin/psx.mjs build src
spago build
```

Launch-adapter work is subject to the explicit implementation-language exception
requested in [ADAPTER.md](ADAPTER.md) and on #68. Grammar registration is usable
independently. The checked-in [settings fixture](test/fixtures/settings.json)
records the intended standard Zed binary and initialization-options shape,
including a separate consumer workspace and a PSX checkout with spaces in its
path. It is a fixture, not a completed LSP integration claim.

## Validation

Python 3.11+ is needed for the manifest validator (`tomllib`). Run from the
repository root using the shared pinned Tree-sitter runtime dependencies:

```sh
python3 -m venv work/zed-venv
work/zed-venv/bin/pip install -r editors/tree-sitter-psx/test/requirements.txt
. work/zed-venv/bin/activate
npm run test:zed
npm run test:tree-sitter
```

The automated gate parses the manifest/configurations, resolves pinned grammar
sources with Git, compiles both grammars with the same include path Zed uses,
compiles every shipped query, and checks highlights, brackets, indentation,
comment scopes and nested PSX/CSS injections on both file kinds without a server.
CI additionally downloads SHA-256-verified **WASI SDK 34.0**, then builds both
Wasm grammars using Zed’s compiler flags. Set `WASI_SDK_PATH` to the extracted
SDK directory to include that check locally. These checks do not substitute for
a Zed GUI smoke test.

The shared server CI gate (`npm run test:language-server` and
`PSX_LSP_REAL=1 npm run test:language-server` after `npx spago build`) covers stdio
framing, mapped diagnostics, completion, hover, definition, formatting and
shutdown independently of any editor. Editor-specific smoke results belong in
[SMOKE.md](SMOKE.md).

## Troubleshooting

- Wrong language: remove conflicting custom `file_types` settings, select PSX or
  PSXT in the language picker, and verify the development extension is installed.
- No highlighting: inspect `zed: open log` for grammar download/build failures;
  verify GitHub access and the full pinned revision, then reinstall the extension.
- Missing CSS: check that CSS support is enabled. PSXT script injections target
  this extension's PSX language, not a separate PureScript extension.
- Comment toggling: PureScript uses `--`/`{- -}`, markup uses `<!-- -->`, and CSS
  uses its injected language's comments. Test inside content, not on boundaries.
- No compiler features: verify the server build, consumer `spago.yaml`, backend
  dependencies, lowered sources, compiler executable and matching output files.
  PSX parse diagnostics and markup completion can work when compiler tools fail;
  read server stderr before treating partial support as a successful compiler test.
