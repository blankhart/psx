# Purescript PSX

Design document: [PSX: framework-agnostic markup for PureScript](https://claude.ai/artifact/QVukPHBzPzQVjJG6NvYSTC#0a3beff7-8da5)

## Development

```sh
npm ci                                  # purs and spago, pinned
npx spago build                         # every package in the workspace
npx spago test                          # every package's tests, including the corpus
npx spago run -p psx-corpus -- --accept # rewrite corpus goldens (optionally: area or area/case)
npm run psx -- build examples/portico/src # lower .psx files to .purs beside them (after spago build)
npm run psx -- check examples/portico/src -- -p psx-examples-portico # compile, errors at .psx positions
npm run psx -- fmt src/View.psx         # format in place (--check: report, write nothing)
npm run examples:portico                # lower, compile and bundle the Portico examples, then run Portico's smoke test
```

The Portico examples in `examples/portico/src` are Portico's own, rewritten in `.psx`; their lowered `.purs` files are committed. `examples/portico/scripts/smoke-test.mjs` is a verbatim copy of Portico's smoke test (CI checks it against the pinned commit); it needs Chrome, found through `CHROME_PATH`.

The toolchain lives in `packages/`, the backends in `backends/`, examples in `examples/`, and the tests in `test/`: corpus cases in `test/corpus/<area>/<case>/` (an `input.psx` or `input.psxt` and one `expected.*` golden per stage), the runner in `test/corpus-runner`, the scenario suite in `test/suite`. `CLAUDE.md` has the rules every change follows.
