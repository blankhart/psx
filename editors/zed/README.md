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
   directory, not the repository root. Install Rust with `rustup` first and make
   `cargo` available to Zed (restart Zed after changing PATH). The checked-in
   toolchain selects Rust 1.90.0 and `wasm32-wasip2`; the adapter pins Zed API
   0.7.0 and commits Cargo.lock. Target Zed 1.27.0 or a compatible newer release.
   Zed fetches the exact grammar commits and
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

## Language server settings

Remove the highlighting-only `enable_language_server: false` settings when ready.
Add the following to the consumer workspace's `.zed/settings.json`. For editing
this PSX checkout, omit `projectRoot` (or point it at this checkout); for a separate
consumer, it must point at the consumer, while the server script remains in the
built PSX checkout. Replace every example path with an existing absolute path.

```json
{
  "languages": {
    "PSX": { "language_servers": ["psx-language-server"] },
    "PSXT": { "language_servers": ["psx-language-server"] }
  },
  "lsp": {
    "psx-language-server": {
      "binary": {
        "path": "/opt/node/bin/node",
        "arguments": [
          "/workspace/PSX checkout/packages/psx-language-server/bin/psx-language-server.mjs",
          "--stdio"
        ]
      },
      "initialization_options": {
        "projectRoot": "/workspace/consumer with spaces",
        "purescriptLanguageServer": "/workspace/PSX checkout/node_modules/.bin/purescript-language-server",
        "purescript": {
          "pursExe": "/workspace/PSX checkout/node_modules/.bin/purs",
          "outputDirectory": "output"
        }
      }
    }
  }
}
```

`binary.arguments` is an argument vector: paths containing spaces are one JSON
string, with no extra shell quotes. `binary.env` optionally supplies environment
variables. With no configured binary path, the adapter looks for a local
`psx-language-server` wrapper on the worktree's PATH and defaults to `--stdio`.
The extension does not download a server. Missing configuration gives an error
with the exact settings keys and build command. Explicit empty arguments remain
empty, allowing a wrapper that handles its own transport arguments.

The same server ID handles both file kinds, using `psx`/`psxt` LSP language IDs.
No `.purs` suffix or PureScript language registration is changed. PSXT scripts
inject PSX for syntax support; do not add the separate PureScript server to PSX
or PSXT. Its compiler integration is already owned by the PSX server. A separate
PureScript extension may continue to handle ordinary `.purs` files.

Use `dev: open language server logs` to inspect PSX's stderr and protocol traffic,
`zed: open log` for extension/build/startup failures, and
`editor: restart language server` after changing command or compiler settings.
Zed owns process startup/restart/shutdown. PSX owns its compiler child and removes
its temporary files on exit; the adapter creates no additional processes.

The approved implementation-language exception and its limits are recorded in
[ADAPTER.md](ADAPTER.md) and `CLAUDE.md`.

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
sources with Git (a full clone, or fetch the pinned revision explicitly), compiles both grammars with the same include path Zed uses,
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

The launch adapter has an additional automated gate:

```sh
cd editors/zed
cargo fmt --check
cargo test --locked
cargo build --release --target wasm32-wasip2 --locked
```

It checks configured commands, space-containing argument vectors, environment
variables, PATH fallback, explicit empty arguments and actionable missing-command
errors. The component build uses Zed's actual `wasm32-wasip2` target.

## Troubleshooting

- Wrong language: remove conflicting custom `file_types` settings, select PSX or
  PSXT in the language picker, and verify the development extension is installed.
- No highlighting: inspect `zed: open log` for grammar download/build failures;
  verify GitHub access and the full pinned revision, then reinstall the extension.
- Missing CSS: check that CSS support is enabled. PSXT script injections target
  this extension's PSX language, not a separate PureScript extension.
- Comment toggling: PureScript uses `--`/`{- -}`, markup uses `<!-- -->`, and CSS
  uses its injected language's comments. Test inside content, not on boundaries.
- Server startup fails: check Node exists and that the script path and built
  `output/Psx.LanguageServer` exist in the PSX checkout. Inspect both logs above;
  a missing executable is a Zed process error, while a missing server build is
  reported on stderr. Restore settings and restart the language server.
- No compiler features: verify the server build, consumer `spago.yaml`, backend
  dependencies, lowered sources, compiler executable and matching output files.
  PSX parse diagnostics and markup completion can work when compiler tools fail;
  read server stderr before treating partial support as a successful compiler test.
