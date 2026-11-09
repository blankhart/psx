import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import textmate from 'vscode-textmate';
import oniguruma from 'vscode-oniguruma';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
await oniguruma.loadWASM(await readFile(require.resolve('vscode-oniguruma/release/onig.wasm')));
const files = {
  'source.psx': '../syntaxes/psx.tmLanguage.json',
  'text.html.psxt': '../syntaxes/psxt.tmLanguage.json',
  'source.purescript': './vendor/purescript.json',
  'source.css': './vendor/css.json',
};
const registry = new textmate.Registry({
  onigLib: Promise.resolve({
    createOnigScanner: patterns => new oniguruma.OnigScanner(patterns),
    createOnigString: value => new oniguruma.OnigString(value),
  }),
  async loadGrammar(scope) {
    assert.ok(files[scope], `Undeclared grammar dependency: ${scope}`);
    const url = new URL(files[scope], import.meta.url);
    return textmate.parseRawGrammar(await readFile(url, 'utf8'), fileURLToPath(url));
  },
});
const cases = JSON.parse(await readFile(new URL('./assertions.json', import.meta.url), 'utf8'));
for (const fixture of cases) {
  test(fixture.file, async () => {
    const grammar = await registry.loadGrammar(fixture.file.endsWith('.psxt') ? 'text.html.psxt' : 'source.psx');
    const lines = (await readFile(new URL(`./fixtures/${fixture.file}`, import.meta.url), 'utf8')).split('\n');
    let state = textmate.INITIAL;
    const tokens = lines.map(line => {
      const result = grammar.tokenizeLine(line, state, 1000);
      assert.equal(result.stoppedEarly, false, `Tokenization timed out: ${line}`);
      state = result.ruleStack;
      return result.tokens;
    });
    for (const check of fixture.checks) {
      const row = check.line === undefined ? lines.findIndex(line => line.includes(check.text)) : check.line - 1;
      assert.ok(row >= 0, `Missing fixture text: ${check.text}`);
      const start = lines[row].indexOf(check.text);
      assert.ok(start >= 0, `Missing text on line ${row + 1}: ${check.text}`);
      const offset = check.offset ?? 0;
      assert.ok(offset >= 0 && offset < check.text.length, `Invalid offset for ${check.text}`);
      const col = start + offset;
      const scopes = tokens[row].find(t => t.startIndex <= col && col < t.endIndex)?.scopes ?? [];
      const message = `${fixture.file}:${row + 1}:${col + 1} ${check.text}: ${scopes.join(' ')}`;
      for (const scope of check.has ?? []) assert.ok(scopes.some(s => s === scope || s.startsWith(`${scope}.`)), `${message} lacks ${scope}`);
      for (const scope of check.lacks ?? []) assert.ok(!scopes.some(s => s === scope || s.startsWith(`${scope}.`)), `${message} unexpectedly has ${scope}`);
    }
  });
}
