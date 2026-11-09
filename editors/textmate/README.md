# PSX TextMate grammars

Syntax highlighting for `.psx` (`source.psx`) and `.psxt`
(`text.html.psxt`), without a language server. The JSON grammars contain only
PSX boundaries and recovery rules. PureScript declarations, types, expressions,
characters, escapes and triple-quoted strings reuse `source.purescript`;
style contents reuse `source.css`. Markup uses standard HTML tag, attribute,
entity and comment scopes, with recursive PSX elements and holes.

## VS Code embedding contract

`extension-contributions.json` is a manifest fragment for the VS Code extension
tracked under #62, not a separately published extension. Merge its fields into
the extension manifest, copy the grammars and both language configurations, and
adjust the paths relative to that manifest. Keep `source.psx` registered even
when opening only PSXT: PSXT includes its repository rules. No `injectTo` entry
or additional injection grammar is needed: each root grammar carries local,
left-priority injections that also work inside the included PureScript grammar.

Declare `nwolverson.language-purescript` as an extension dependency (tested with
0.2.10); it supplies `source.purescript`. VS Code's built-in CSS extension
supplies `source.css`. Other TextMate hosts must register both dependencies.
The PureScript grammar's repository rules `string_triple_quoted`,
`string_single_quoted` and `characters` are part of this dependency contract.
No upstream grammar files need to ship in the PSX extension.

Stable PSX scopes and language mappings:

| Scope | Role / embedded language |
| --- | --- |
| `source.psx` | PureScript outer module with markup literals |
| `text.html.psxt` | HTML document with PureScript script and template equations |
| `meta.markup.psx` | Markup literal or template |
| `meta.element.psx`, `meta.fragment.psx`, `meta.tag.void.psx` | Element, fragment, void element |
| `meta.embedded.expression.psx` | Child, attribute or trait hole; `purescript` |
| `meta.braces.psx` | Balanced PureScript record braces within holes |
| `meta.embedded.block.purescript.psxt` | `script type="text/purescript"`; `purescript` |
| `meta.embedded.inline.purescript.psxt` | Template `head` binders and guards; `purescript` |
| `meta.embedded.block.css.psx` | Scoped style body; `css` |
| `meta.embedded.block.raw.psx` | Raw script, textarea or title body |
| `punctuation.section.embedded.begin.psx`, `punctuation.section.embedded.end.psx` | Hole delimiters |

Use the supplied `embeddedLanguages` and `tokenTypes` mappings so VS Code treats
holes and heads as code. They support editor services; they do not start a
language server. CSS is included directly within the style scope, so it needs
no separate injection registration.

Language configurations supply PureScript comments for PSX, HTML comments for
PSXT, brace/parenthesis/bracket pairing, indentation folding for PSX, and
`-- #region` / `<!-- #region -->` folding markers. VS Code language configurations
cannot switch comment commands by embedded scope: the extension may provide
context-aware commands later. Angle brackets are intentionally not auto-paired
because `<` is also an operator.

## Boundaries and recovery

A markup literal needs an ASCII-letter tag name or `<>` after `<`. In PureScript it
also needs a preceding expression-start punctuation/operator, `then`, `else`,
`of`, `in`, or the beginning of an indented line. Thus `a < b`, `a<b`, and `a <> b`
remain PureScript. Elements recurse, and each `{...}` hole balances record
braces before returning to markup. Strings, characters and comments shield
braces and tag-like text. Template heads embed ordinary PureScript, including
destructuring and guards, without treating `<` comparisons as markup.

TextMate cannot reproduce the CST parser's layout or operator state. A tag at
line start can be mistaken for a literal after a continued comparison, and an
operator immediately followed by a tag-like sequence can be ambiguous. This
is highlighting only; the parser remains authoritative. Tag matching follows the lexer's ASCII name characters and is case-sensitive.
Capitalized names are tags, with no component lookup semantics.

Ordinary quoted strings recover at line end; explicit PureScript string gaps
continue on the following line. Incomplete closing tags recover at line end.
Markup, holes, and block comments recover at a new unindented lowercase
identifier equation, or a PSXT closing script/template tag. This bounds common
malformed edits, but an equation-looking line inside a valid multiline comment
can cause early recovery. Nested block comments still work. Triple strings and
CSS retain their upstream multiline rules: an unclosed triple string or CSS
comment can keep its scope until its delimiter is supplied. TextMate cannot
reliably infer a missing delimiter. These heuristics never change PSX parsing.

## Tests and upstream licenses

From the repository root:

```sh
npm ci
npm run test:textmate
```

CI runs the same command with `vscode-textmate` 9.2.0 and `vscode-oniguruma`
2.0.1 pinned in the workspace lockfile. Tests assert positive and negative
scopes at boundaries, and fail on undeclared grammar dependencies or a
tokenization timeout. Fixtures `nested-holes.psx`, `trait-holes.psx`,
`raw-text.psx`, `comments.psx`, `void-elements.psx`, `svg.psx`,
`equations.psxt` and `styles.psxt` are copied from the matching
`test/corpus/parse/*` and `test/corpus/psxt/*` inputs. `boundaries.psx` and
`incomplete.*` exercise operators, escaping, nesting, and partial edits.
Fixtures and assertions were added before the grammar rules.

Tests use unmodified, vendored upstream grammars, with no download in CI:

- `test/vendor/purescript.json`: `nwolverson/vscode-language-purescript`,
  commit `74f436686b47f33bb4f9dcf4aa6f714149192260`, `syntaxes/purescript.json`.
  MIT; `PureScript-LICENSE.md` includes the upstream Atom PureScript and Haskell
  notices. The recovery string rule also reuses that grammar's escape rules.
- `test/vendor/css.json`: `microsoft/vscode`, tag 1.105.1, commit
  `7d842fb85a0275a4a8e4d7e040d2625abbf7f084`,
  `extensions/css/syntaxes/css.tmLanguage.json`. MIT; `VSCode-LICENSE.txt`.
  Its embedded provenance names the original `microsoft/vscode-css` commit.

When updating a dependency, replace the grammar from an immutable revision,
retain its license notices, update these pins, and run the scope assertions.
The vendor directory is test data, not a second runtime implementation of
PureScript or CSS highlighting.
