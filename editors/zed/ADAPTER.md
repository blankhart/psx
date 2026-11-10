# Zed launch adapter decision (#68)

Zed's declarative extension manifest registers languages, grammar sources and
language-server associations. Its `LanguageServerManifestEntry` has no command,
arguments or initialization-options implementation. The documented launch hook
is Rust `Extension::language_server_command`, compiled to Wasm. User `lsp` settings
can override a registered adapter's binary but do not supply an extension adapter.

References inspected on 2026-11-10:
- https://github.com/zed-industries/zed/blob/main/docs/src/extensions/languages.md#language-servers
- https://github.com/zed-industries/zed/blob/main/crates/extension/src/extension_manifest.rs

## Approved exception

The user approved this specific exception on 2026-11-10, after the request was
posted on #68. `CLAUDE.md` now records its scope.

Permit only `editors/zed/src/lib.rs` and its Cargo build metadata to use Rust as
Zed's required Wasm ABI adapter. The adapter will register the PSX server, resolve
a configured local executable and argument vector (preserving spaces), and pass
initialization options from Zed settings to the existing stdio server. It will
report a useful error when no local server is configured. It will not download a
server, implement protocol behavior, parse/lower PSX, or run compiler logic.
All server behavior remains in the existing PureScript package on Node.

`CLAUDE.md` previously said: “Implementation language is PureScript on Node.
Editor grammars and the VS Code client are the only exceptions.” Issue #68
explicitly requires requesting this exception before adding a Rust adapter.
