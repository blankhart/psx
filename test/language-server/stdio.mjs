import { spawn } from 'node:child_process';
import assert from 'node:assert/strict';
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
  await request('shutdown',null); send('exit');
  const code = await new Promise(resolve => child.on('exit',resolve)); assert.equal(code,0);
  console.log('language-server stdio integration passed');
} finally { child.kill(); }
