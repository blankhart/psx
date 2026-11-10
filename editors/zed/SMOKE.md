# Zed smoke procedure and results

Record the Zed version, platform, commit, server command and workspace path in
any PR completing #68. Keep GUI results separate from the automated gate.

## Procedure

1. Install the development extension as documented in README.md. Open both
   fixtures with language servers disabled. Verify the language picker, markup,
   PureScript and CSS highlights, nested holes, brackets, indentation, and
   comment toggling in each scope.
2. Once the launch adapter is available, enable only `psx-language-server` for
   PSX and PSXT. Configure Node and its script as separate argument-vector
   entries. Use a PSX checkout and a separate consumer workspace with spaces in
   their paths. Build the consumer with the same purs as the server.
3. In each file kind, verify a broken tag reports a PSX source diagnostic and
   repairing it clears that diagnostic. Insert a type error inside a hole and
   confirm the compiler diagnostic points into the original source file.
4. Complete a tag/attribute, then a PureScript variable inside a hole. Hover the
   variable and go to its source definition; no virtual temporary file should
   be opened. Format the document; confirm rendered text and Unicode survive.
5. Restart using `editor: restart language server`. Repeat a feature request,
   close the workspace and confirm the server and its compiler child exit.
6. Open an ordinary `.purs` file with the separate PureScript extension enabled.
   Confirm that extension owns ordinary PureScript while PSX owns PSX and PSXT;
   injected script text must not start a second compiler server on the PSXT URI.
7. Configure a nonexistent server executable, inspect the actionable startup
   error/log, restore settings, and verify restart recovers.

## Results in this environment

- Automated native grammar/query gate: passed on Linux with Python 3.12.14,
  tree-sitter 0.25.2 and tree-sitter-css 0.25.0; no server process involved.
- Zed 1.27.0 (`c84af5b99cbc366d675064e52b23664a015b6e35`) downloaded and
  `--version` verified. GUI startup results are recorded below when available.
- Both grammar Wasm builds: passed with WASI SDK 34.0 and the Zed build flags.
- LSP adapter and editor feature smoke: pending the explicit Rust adapter
  exception. These are not claimed as passing.
- GUI smoke blocked: `zed-editor --system-specs` reports `OS: Linux Headless
  debian 13`, no Vulkan information is available, and this executor exposes no
  desktop JavaScript/control tool. Starting the editor did not provide an
  inspectable window. Recognition, colours and editor actions remain unverified
  in Zed itself; the native automated checks above are the evidence available.
