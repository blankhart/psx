# Tree-sitter PSX and PSXT

Two languages are exposed from this directory:

| Language | Grammar directory | Extension | Injection name |
| --- | --- | --- | --- |
| `psx` | `.` | `.psx` | `psx` |
| `psxt` | `psxt` | `.psxt` | `psxt` |

Both have a `purescript` root node (inherited from the base grammar). PSX parses
PureScript and markup together, including PureScript inside child, attribute and
trait holes. Markup is an expression-start production, excluded from function
application. `<` and `<>` remain operators after expressions; `(<>)` remains an
operator value. Tags are lowercase, void HTML names need no end tag, and the
external scanner matches closing names. An ancestor close emits an explicit
`unclosed_tag` recovery node for the missing inner close, retaining subsequent
declarations. Markup text owns whitespace, so text at
column zero never closes a PureScript layout. Raw script/style bodies keep
braces and `<` as text. PureScript layout inside a hole can end at its `}`.

PSXT parses the outer script/template/style document. Script content is injected
as **PSX**, since that module may itself contain markup; style content is injected
as CSS. Template heads contain structural PureScript binders and guards, including
record/constructor destructuring. Both HTML quote styles are supported. The
scanner remembers the head's delimiter so opposite-quoted PureScript string and
character binders stay inside the head. Foreign-import resolution, binder arity,
equation ordering, backend choice and type checking belong to the PSX compiler.
This grammar does not duplicate those semantic checks.

## Reuse and licensing

The base is `postsolar/tree-sitter-purescript`, MIT, pinned at
`f541f95ffd6852fbbe88636317c613285bc105af` (also in
`vendor/purescript/REVISION`). Its modular grammar and layout scanner allow an
expression-start extension without changing upstream. Other PureScript grammar
forks were considered; this one provides the layout support needed by the PSX
corpus. Only generator inputs, the scanner, Unicode table and license are
vendored. These vendored inputs are unchanged; the wrapper omits stale upstream inline
references that are not actual rules. The PSX grammar overrides and scanner wrapper are
local; no purs, Spago, registry or framework changes are required. The wrapper
also clears the base scanner's empty deserialization state, needed for edits.
Keep the upstream MIT notice in `vendor/purescript/LICENSE` when redistributing.

## Generate and test

The root lockfile pins **tree-sitter-cli 0.25.10**, generating **ABI 15** parsers.
The native test runtime is **tree-sitter 0.25.2** (Python bindings); CSS injection
checks use **tree-sitter-css 0.25.0**. Use a Tree-sitter runtime supporting ABI 15.
Generated `src/parser.c`, `src/grammar.json`, `src/node-types.json` and headers in
both grammar directories are committed. Generation is offline after `npm ci`;
the upstream repository is not fetched.

From the repository root, with Node, Python 3.10+ and a C compiler installed:

```sh
npm ci
python3 -m venv work/tree-sitter-venv
work/tree-sitter-venv/bin/pip install -r editors/tree-sitter-psx/test/requirements.txt
npm run generate:tree-sitter
work/tree-sitter-venv/bin/python editors/tree-sitter-psx/test/test.py
```

`npm run test:tree-sitter` uses `python3`; activate the virtual environment first
when running it locally. CI regenerates both parsers, checks the generated files
are unchanged, and runs the native tests. Tests compile shared libraries in a
temporary directory; no editor or Node native addon is required.

`test/fixtures.json` enumerates **every** current PSX corpus input by its existing
`expected.parse` outcome, not its filename: some `lower/error-*` inputs are valid
syntax, while `fmt/edges` deliberately has an invalid markup argument. Valid
inputs must have neither ERROR nor missing nodes (including hidden missing
tokens). Invalid parser and semantic cases have individually reviewed recovery
trees in `test/recovery.json`; they do not use a blanket no-error assertion.
Run the test with `--accept` to update those trees and review the diff. Additional
boundary fixtures live in `test/cases.json`. Tests also compile every query,
assert operator/tag/head captures, parse injected script and CSS bodies, and
compare incremental edits with fresh parses before and after repairing broken
tags and holes, including UTF-8 edits.

## Editor integration

`tree-sitter.json` advertises both languages and their highlight/injection paths.
Compile each directory's `src/parser.c` and `src/scanner.c` with its `src` include
path **and this directory's root include path**. Keep the directory layout:
scanners include the shared `scanner.h` and pinned vendor files. The scanner uses
C11 and the standard C library; no external runtime dependency is needed beyond
Tree-sitter. Each language exports `tree_sitter_psx` or `tree_sitter_psxt`.

Highlight queries adapt the upstream scopes to Zed theme capture names.
Queries use Zed-compatible highlight names and `injection.content` /
`injection.language` captures/settings. Each grammar directory has:

- `highlights.scm`: PureScript and markup scopes; head binders and guards in PSXT.
- `injections.scm`: CSS styles; PSXT PureScript scripts, and PSX JavaScript scripts.
- `brackets.scm`: parentheses/braces/brackets, paired tags and fragments.
- `indents.scm`: paired element/fragment/hole boundaries, plus PSXT templates.
- `folds.scm`: `@fold` ranges for consumers supporting folding queries (such as
  Neovim), covering elements, fragments, holes, raw blocks and PSXT templates.
  Zed uses its indentation-based folding; it does not load `folds.scm`.

For Zed, register two grammar entries pointing to these directories and copy the
corresponding query files into the extension's language directories. Register
`psx` as an injection language and ensure CSS is available. The Zed extension is
a separate issue; these files do not install an extension themselves.
