const assert = require('node:assert/strict');
const fs = require('node:fs');

// Imports used by these scanners and supplied by Zed 1.27's Tree-sitter
// runtime. Keep this narrow so new libc dependencies get reviewed.
const supported = new Map([
  ['memory', 'memory'],
  ['__indirect_function_table', 'table'],
  ['__stack_pointer', 'global'],
  ['__memory_base', 'global'],
  ['__table_base', 'global'],
  ...['calloc', 'realloc', '__assert_fail', 'free', 'strlen', 'iswspace',
    'iswalnum', 'strcmp', 'memcpy', 'iswlower'].map(name => [name, 'function']),
]);

for (const path of process.argv.slice(2)) {
  const module = new WebAssembly.Module(fs.readFileSync(path));
  for (const entry of WebAssembly.Module.imports(module)) {
    assert(entry.module === 'env' && supported.get(entry.name) === entry.kind,
      `${path}: unsupported Zed Tree-sitter import ${entry.module}.${entry.name} (${entry.kind})`);
  }
  console.log(`Checked Zed Wasm imports: ${path}`);
}
