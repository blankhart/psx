import { spawn, spawnSync } from 'node:child_process';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { tmpdir } from 'node:os';
import { pathToFileURL } from 'node:url';
const child = spawn(process.execPath, [path.resolve('packages/psx-language-server/bin/psx-language-server.mjs')],{cwd:tmpdir()});
let input = Buffer.alloc(0), seq = 0;
const pending = new Map(), notifications = [];
child.stderr.on('data', b => process.stderr.write(b));
child.stdout.on('data', b => {
  input = Buffer.concat([input, b]);
  while (true) {
    const end = input.indexOf('\r\n\r\n'); if (end < 0) break;
    const length = Number(/Content-Length: (\d+)/i.exec(input.subarray(0,end).toString())[1]);
    if (input.length < end + 4 + length) break;
    const m = JSON.parse(input.subarray(end+4,end+4+length)); input = input.subarray(end+4+length);
    if ('id' in m) { pending.get(m.id)?.(m); pending.delete(m.id); } else notifications.push(m);
  }
});
function send(method, params, id) {
  const b = Buffer.from(JSON.stringify({jsonrpc:'2.0',method,params,...(id === undefined ? {} : {id})}));
  child.stdin.write(`Content-Length: ${b.length}\r\n\r\n`); child.stdin.write(b);
}
function request(method, params) {
  const id = ++seq;
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(Error(`timeout: ${method}`)), 10000);
    pending.set(id, m => {clearTimeout(timer); m.error ? reject(Error(JSON.stringify(m.error))) : resolve(m.result);});
    send(method, params, id);
  });
}
try {
  const init = await request('initialize', {processId:null,capabilities:{},rootUri:pathToFileURL(process.cwd()).href, initializationOptions:process.env.PSX_LSP_REAL ? {purescript:{outputDirectory:process.env.PSX_LSP_OUTPUT || 'output', ...(process.env.PSX_LSP_PURS ? {pursExe:process.env.PSX_LSP_PURS} : {})}} : {purescriptLanguageServer:'/nonexistent/psx-test-pls'}});
  assert.equal(init.capabilities.positionEncoding, 'utf-16');
  if (process.env.PSX_LSP_REAL) for (const ext of ['psx','psxt']) {
    const uri = pathToFileURL(`/tmp/PsxLspReal.${ext}`).href;
    const script = 'module PsxLspReal where\n\nimport Prelude\nimport Psx.Static\n\nlabel :: String\nlabel = show unit\nview = <div>😀 {label}</div>\n';
    const text = ext === 'psx' ? script : '<script type="text/purescript">\n' + script.replace('view = <div>😀 {label}</div>', 'foreign import view :: Markup Unit') + '</script>\n<template head="view"><div>😀 {label}</div></template>\n';
    const lines = text.split('\n'), holeLine = lines.findIndex(l => l.includes('{label}')), holeColumn = lines[holeLine].indexOf('{label}')+2;
    const definitionLine = lines.findIndex(l => l.startsWith('label ='));
    send('textDocument/didOpen',{textDocument:{uri,languageId:'psx',version:1,text}});
    let hover;
    for (let attempt=0; attempt<80; attempt++) {
      hover = await request('textDocument/hover',{textDocument:{uri},position:{line:holeLine,character:holeColumn}});
      if (hover) break;
      await new Promise(r => setTimeout(r,250));
    }
    assert.ok(hover, 'real compiler hover in hole');
    const completion = await request('textDocument/completion',{textDocument:{uri},position:{line:holeLine,character:holeColumn+1}});
    assert.ok(completion.items.some(i => i.label.startsWith('label')), 'compiler completion in hole');
    const defs = await request('textDocument/definition',{textDocument:{uri},position:{line:holeLine,character:holeColumn}});
    assert.ok(defs.some(d => d.uri === uri && d.range.start.line === definitionLine), 'definition maps back to source');
    const ordinary = await request('textDocument/definition',{textDocument:{uri},position:{line:definitionLine,character:15}});
    assert.ok(ordinary.some(d => d.uri?.endsWith('.purs') && !d.uri.includes('psx-lsp-')), 'ordinary PureScript target preserved');
    send('textDocument/didChange',{textDocument:{uri,version:2},contentChanges:[{text:text.replace('{label}','{label + 1}')}]});
    for (let attempt=0; attempt<80 && !notifications.some(n => n.params?.uri === uri && n.params.version === 2 && n.params.diagnostics.some(d => d.source === 'purescript')); attempt++) await new Promise(r => setTimeout(r,250));
    assert.ok(notifications.some(n => n.params?.uri === uri && n.params.version === 2 && n.params.diagnostics.some(d => d.source === 'purescript' && d.range.start.line === holeLine)), 'type error maps into hole');
    send('textDocument/didClose',{textDocument:{uri}});
  }
  for (const ext of ['psx','psxt']) {
    const uri = pathToFileURL(`/tmp/psx-lsp-test.${ext}`).href;
    const script = 'module LspTest where\r\n\r\nimport Psx.Static\r\n\r\nforeign import view :: String\r\n';
    const text = ext === 'psx' ? 'module LspTest where\r\n\r\nimport Psx.Static\r\n\r\nview = <div>😀 {"é"}</div>\r\n' : `<script type="text/purescript">\r\n${script}</script>\r\n<template head="view"><div>😀 {"é"}</div></template>\r\n`;
    send('textDocument/didOpen',{textDocument:{uri,languageId:'psx',version:1,text}});
    const edits = await request('textDocument/formatting',{textDocument:{uri},options:{tabSize:2,insertSpaces:true}});
    assert.equal(edits.length,1); assert.match(edits[0].newText,/😀/);
    const line = text.split('\n').findIndex(l => l.includes('<div>'));
    const completions = await request('textDocument/completion',{textDocument:{uri},position:{line,character:text.split('\n')[line].indexOf('<div>')+2}});
    assert.ok(completions.items.some(i => i.label === 'div'));
    const hover = await request('textDocument/hover',{textDocument:{uri},position:{line,character:0}});
    if (!process.env.PSX_LSP_REAL) assert.equal(hover,null);
    if (ext === 'psxt') {
      const definitions = await request('textDocument/definition',{textDocument:{uri},position:{line:5,character:17}});
      assert.ok(definitions.length);
    }
    send('textDocument/didChange',{textDocument:{uri,version:2},contentChanges:[{text:'module Broken where\nx = <div>'}]});
    await request('textDocument/hover',{textDocument:{uri},position:{line:0,character:0}});
    assert.ok(notifications.some(n => n.params?.uri === uri && n.params.version === 2 && n.params.diagnostics.length));
    const since = notifications.length;
    send('textDocument/didChange',{textDocument:{uri,version:3},contentChanges:[{text}]});
    await request('textDocument/hover',{textDocument:{uri},position:{line:0,character:0}});
    await new Promise(r => setTimeout(r,300));
    const recent = notifications.slice(since).filter(n => n.params?.uri === uri);
    assert.ok(recent.some(n => n.params.version === 3 && !n.params.diagnostics.some(d => d.source === 'psx' && d.severity === 1)), 'resolved parse errors cleared');
    assert.ok(recent.every(n => n.params.version === 3), 'stale diagnostics discarded');
    send('textDocument/didSave',{textDocument:{uri}});
    send('textDocument/didClose',{textDocument:{uri}});
  }
  // The server runs from tmpdir; discovery must follow the document, not cwd.
  const configRoot = fs.mkdtempSync(path.join(tmpdir(), 'psx tidy-'));
  try {
    const nested = path.join(configRoot, 'src'); fs.mkdirSync(nested);
    const file = path.join(nested, 'Configured.psxt'), uri = pathToFileURL(file).href;
    const script = 'module Configured where\n\nidentity :: forall a. a -> a\nidentity value = value\n\nvalues = [ firstLongValue, secondLongValue, thirdLongValue, fourthLongValue, fifthLongValue ]\n';
    const text = '<script type="text/purescript">\n' + script + '</script>\n';
    const cli = path.resolve('packages/psx-cli/bin/psx.mjs');
    const runCli = () => spawnSync(process.execPath, [cli, 'fmt', file], {encoding:'utf8'});
    const config = path.join(configRoot, '.tidyrc.json');
    const editor = async (version, content) => {
      if (version === 1) send('textDocument/didOpen',{textDocument:{uri,languageId:'psxt',version,text:content}});
      else send('textDocument/didChange',{textDocument:{uri,version},contentChanges:[{text:content}]});
      return (await request('textDocument/formatting',{textDocument:{uri},options:{tabSize:2,insertSpaces:true}}))[0].newText;
    };
    fs.writeFileSync(path.join(configRoot, 'operators'), 'Prelude.(<>) 5\n');
    let version = 0;
    for (const settings of [null, {width:80,unicode:'always',indent:4}, {width:40,unicode:'never',indent:2,ribbon:0.8,typeArrowPlacement:'last',importSort:'ide',importWrap:'auto',operatorsFile:'operators'}]) {
      if (settings) fs.writeFileSync(config, JSON.stringify(settings));
      fs.writeFileSync(file, text);
      const result = runCli(); assert.equal(result.status, 0, result.stderr);
      const formatted = fs.readFileSync(file, 'utf8');
      assert.equal(await editor(++version, text), formatted, 'CLI/editor configuration agrees');
      assert.equal(await editor(++version, formatted), formatted, 'editor second pass stable');
      assert.equal(runCli().status, 0); assert.equal(fs.readFileSync(file,'utf8'), formatted, 'CLI second pass stable');
      if (settings) {
        assert.ok(!formatted.includes('values = ['), 'configured width wraps the array');
        assert.equal(formatted.includes('∀'), settings.unicode === 'always', 'unicode option honored');
      } else assert.ok(formatted.includes('values = ['), 'no-config width is unlimited');
      // Compare the embedded script directly with purs-tidy using the same config.
      if (settings) {
        const purs = path.join(nested, 'Configured.purs'); fs.writeFileSync(purs,script);
        const tidy = spawnSync(process.execPath,[path.resolve('node_modules/purs-tidy/bin/index.js'),'format-in-place',purs],{cwd:configRoot,encoding:'utf8'});
        assert.equal(tidy.status,0,tidy.stderr);
        assert.equal(formatted.match(/^<script[^>]*>\n([\s\S]*)<\/script>\n$/)[1], fs.readFileSync(purs,'utf8'));
      }
    }
    // Nearest config overrides the parent, and changes are read on each request.
    fs.writeFileSync(path.join(nested,'.tidyrc.json'), '{"unicode":"always"}');
    assert.ok((await editor(++version,text)).includes('∀'));
    for (const invalid of ['{', '{"width":"eighty"}', '{"unicode":"invalid"}', '{"operatorsFile":"missing"}']) {
      fs.writeFileSync(path.join(nested,'.tidyrc.json'),invalid);
      fs.writeFileSync(file,text);
      assert.notEqual(runCli().status,0);
      assert.equal(fs.readFileSync(file,'utf8'),text,'configuration failure leaves source unchanged');
      await assert.rejects(editor(++version,text));
    }
    send('textDocument/didClose',{textDocument:{uri}});
  } finally { fs.rmSync(configRoot,{recursive:true,force:true}); }
  await request('shutdown',null); send('exit');
  const code = await new Promise(resolve => child.on('exit',resolve)); assert.equal(code,0);
  console.log('language-server stdio integration passed');
} finally { child.kill(); }
