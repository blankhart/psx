# PSX language server

From a clean checkout run `npm ci && npx spago build`. Launch with
`node /absolute/path/to/psx/packages/psx-language-server/bin/psx-language-server.mjs --stdio`.
The process uses LSP Content-Length framing on stdin/stdout and logs only to stderr.
Node 22, purs 0.15.15 and Spago 1.0.5 are required to build.

Clients send `rootUri` (or the first workspace folder) and may set initializationOptions:
`projectRoot` (absolute path), `purescriptLanguageServer` (executable path, default
the bundled `purescript-language-server`), and `purescript` (settings passed to that server).
The launch entry uses the bundled purescript-language-server by default. Build the
consumer sources with `node /absolute/path/to/psx/packages/psx-cli/bin/psx.mjs build src`
and then `spago build` before requesting compiler features. An optional
replacement executable must be on PATH or configured explicitly. Configuration does not depend on the launch working directory.
The nearest ancestor of the root with spago.yaml is the default project root.

PSX owns one child language server per session. Virtual lowered buffers live in a
private temporary directory outside source globs. They are sent as unsaved `.purs`
buffers to the child, never written beside user sources. Parse/lowering diagnostics
update on every edit; compiler diagnostics run on open and each valid edit through the child server's
no-filesystem diagnostic support. Each revision has a new URI so unversioned
compiler responses cannot attach to a newer buffer. Initial compiler setup and backend dependencies belong to the
consumer workspace. Missing compiler tools leave markup completion, formatting and
parse diagnostics available; stderr explains the failure. Generated-only spans are
suppressed rather than guessed. Closing a document clears diagnostics; exit kills
the child and removes temporary files.

Run `node test/language-server/stdio.mjs` after building for the protocol gate.

The server reuses vscode-languageserver for protocol and incremental buffers and
purescript-language-server for compiler operations. PureScript calls the existing
PSX lowering, formatter and HTML-data packages through a Node IO adapter.

For compiler integration checks run `PSX_LSP_REAL=1 npm run test:language-server`.
`PSX_LSP_OUTPUT` overrides the compiler output directory for this test. If externs
versions mismatch, build dependencies with the same purs executable configured in
`initializationOptions.purescript.pursExe`.

Scoped class suggestions use styles attached to the cursor’s ancestor elements;
top-level `.psxt` styles apply to every template. This first server intentionally
suppresses edits and diagnostics in generated-only spans. Compiler completion
import edits that cannot be mapped safely are also suppressed.
