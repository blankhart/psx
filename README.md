# Purescript PSX

Design document: [PSX: framework-agnostic markup for PureScript](https://claude.ai/artifact/QVukPHBzPzQVjJG6NvYSTC#0a3beff7-8da5)

## Development

```sh
npm ci                                  # purs and spago, pinned
npx spago build                         # every package in the workspace
npx spago test                          # every package's tests, including the corpus
npx spago run -p psx-corpus -- --accept # rewrite corpus goldens (optionally: area or area/case)
```

The toolchain lives in `packages/`, the backends in `backends/`, and the tests in `test/`: corpus cases in `test/corpus/<area>/<case>/` (an `input.psx` or `input.psxt` and one `expected.*` golden per stage), the runner in `test/corpus-runner`, the scenario suite in `test/suite`. `CLAUDE.md` has the rules every change follows.
