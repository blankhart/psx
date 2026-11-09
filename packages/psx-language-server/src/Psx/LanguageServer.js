import { createConnection, TextDocuments, TextDocumentSyncKind, ResponseError, LSPErrorCodes } from 'vscode-languageserver/node.js';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { createMessageConnection, StreamMessageReader, StreamMessageWriter } from 'vscode-jsonrpc/node.js';
import { createServer } from 'node:net';
import { spawn } from 'node:child_process';
import { mkdtempSync, rmSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

// Node IO adapter. Protocol framing, request errors, cancellation and incremental
// buffer updates are supplied by the established VS Code LSP implementation.
export const serve = analyze => format => completionData => stylesAt => () => {
  const connection = createConnection(process.stdin, process.stdout);
  const documents = new TextDocuments({
    create: TextDocument.create,
    update: (document, changes, version) => version > document.version ? TextDocument.update(document, changes, version) : document
  });
  const html = JSON.parse(completionData), virtuals = new Map(), reverse = new Map();
  const temp = mkdtempSync(path.join(tmpdir(), 'psx-lsp-'));
  let child, proxy, ready = Promise.resolve(false), shuttingDown = false, serial = 0;
  const log = message => process.stderr.write(`[psx] ${message}\n`);
  const cleanup = () => { proxy?.dispose(); child?.kill(); rmSync(temp,{recursive:true,force:true}); };
  process.once('exit', cleanup);
  process.once('SIGTERM', async () => {
    if (proxy) {try {await Promise.race([proxy.sendRequest('shutdown'),new Promise(r => setTimeout(r,2000))]);} catch {}}
    cleanup(); process.exit(0);
  });
  const uriPath = uri => { try { return fileURLToPath(uri); } catch { return process.cwd(); } };
  const projectRoot = params => {
    let root = params.initializationOptions?.projectRoot || uriPath(params.rootUri || params.workspaceFolders?.[0]?.uri || pathToFileURL(process.cwd()).href);
    for (let dir = root;; dir = path.dirname(dir)) {
      if (existsSync(path.join(dir,'spago.yaml'))) return dir;
      if (dir === path.dirname(dir)) return root;
    }
  };
  const lspPosition = (text, p) => ({line:p.line,character:Array.from(text.split('\n')[p.line] || '').slice(0,p.column).join('').length});
  function mapPosition(v,p,inverse=false) {
    const mapped = (inverse ? v.backward : v.forward)(p);
    return mapped.valid ? mapped.position : null;
  }
  function mapRange(v,r,inverse=true) {
    const start = mapPosition(v,r.start,inverse), end = mapPosition(v,r.end,inverse);
    return start && end && (end.line > start.line || end.line === start.line && end.character >= start.character) ? {start,end} : null;
  }
  function mapEditRange(v,range) {
    const mapped = mapRange(v,range);
    if (!mapped) return null;
    const generated = TextDocument.create(v.generated,'purescript',v.version,v.purs);
    const original = TextDocument.create(v.uri,'psx',v.version,v.text);
    // An edit must replace copied text only; endpoints alone could span markup.
    return generated.getText(range) === original.getText(mapped) ? mapped : null;
  }
  const publish = v => connection.sendDiagnostics({uri:v.uri,version:v.version,diagnostics:[...v.diagnostics,...v.compiler]});
  connection.onInitialize(params => {
    const root = projectRoot(params), options = params.initializationOptions || {};
    ready = (async () => {
      const port = await new Promise((resolve,reject) => {
        const socket = createServer(); socket.on('error',reject);
        socket.listen(0,'127.0.0.1',() => {const port=socket.address().port;socket.close(() => resolve(port));});
      });
      return new Promise(resolve => {
      let settled = false;
      const timer = setTimeout(() => {log('PureScript initialization timed out');done(false);child?.kill();},15000); timer.unref();
      const done = value => {if (!settled) {settled=true;clearTimeout(timer);resolve(value);}};
      child = spawn(options.purescriptLanguageServer || process.execPath,options.purescriptLanguageServer ? ['--stdio'] : [fileURLToPath(import.meta.resolve('purescript-language-server/server.js')),'--stdio'],{cwd:root,stdio:['pipe','pipe','pipe']});
      child.stderr.on('data', b => log(b.toString().trimEnd()));
      child.on('error', error => {log(`PureScript features unavailable: ${error.message}`);done(false);});
      child.on('exit', () => {done(false); proxy = undefined;});
      child.once('spawn', () => {
      proxy = createMessageConnection(new StreamMessageReader(child.stdout),new StreamMessageWriter(child.stdin));
      const settings = {addNpmPath:true, addSpagoSources:true, ...options.purescript, pscIdePort:port, noFsDiagnostics:true, diagnosticsOnOpen:true, diagnosticsOnType:true, fullBuildOnSave:false, fastRebuild:false};
      proxy.onRequest('workspace/configuration', p => p.items.map(() => settings));
      proxy.onNotification('textDocument/publishDiagnostics', p => {
        const uri = reverse.get(p.uri), v = virtuals.get(uri);
        if (!v || !v.valid || p.version != null && p.version !== v.version) return;
        v.compiler = p.diagnostics.flatMap(d => { const range = mapRange(v,d.range); return range ? [{...d,range,source:'purescript'}] : []; });
        publish(v);
      });
      proxy.onRequest('window/showMessageRequest', p => {log(p.message);return null;});
      proxy.onNotification('window/logMessage', p => log(p.message));
      proxy.onNotification('window/showMessage', p => log(p.message));
      proxy.listen();
      proxy.sendRequest('initialize',{...params,rootUri:pathToFileURL(root).href,processId:process.pid,initializationOptions:options.purescript || {}}).then(() => {
        proxy.sendNotification('initialized',{});
        proxy.sendNotification('workspace/didChangeConfiguration',{settings:{purescript:settings}}); done(true);
      }, error => {log(`PureScript initialization failed: ${error.message}`);done(false);});
      });
    });
    })().catch(error => {log(error.message);return false;});
    return {capabilities:{positionEncoding:'utf-16',textDocumentSync:{openClose:true,change:TextDocumentSyncKind.Incremental,save:{includeText:true}},documentFormattingProvider:true,completionProvider:{triggerCharacters:['<',' ','"','.']},hoverProvider:true,definitionProvider:true},serverInfo:{name:'psx-language-server'}};
  });
  async function update(doc) {
    const old = virtuals.get(doc.uri), result = analyze(doc.uri)(doc.getText());
    const generated = pathToFileURL(path.join(temp,`${++serial}.purs`)).href;
    const v = {...result,uri:doc.uri,generated,text:doc.getText(),version:doc.version,compiler:[]};
    v.diagnostics = result.diagnostics.map(d => ({range:{start:lspPosition(v.text,d.range.start),end:lspPosition(v.text,d.range.end)},message:d.message,severity:result.valid ? 2 : 1,source:'psx'}));
    if (old) { reverse.delete(old.generated); if (old.opened && proxy) proxy.sendNotification('textDocument/didClose',{textDocument:{uri:old.generated}}); }
    virtuals.set(doc.uri,v); reverse.set(generated,doc.uri); publish(v);
    if (await ready && virtuals.get(doc.uri) === v && proxy) {
      if (v.valid) {
        proxy.sendNotification('textDocument/didOpen',{textDocument:{uri:generated,languageId:'purescript',version:v.version,text:v.purs}}); v.opened=true;
      }
    }
  }
  documents.onDidOpen(e => {void update(e.document);});
  documents.onDidChangeContent(e => {if (virtuals.get(e.document.uri)?.version !== e.document.version) void update(e.document);});
  documents.onDidSave(async e => {
    const v = virtuals.get(e.document.uri);
    if (await ready && v?.valid && v.opened && proxy) proxy.sendNotification('textDocument/didSave',{textDocument:{uri:v.generated},text:v.purs});
  });
  documents.onDidClose(e => {
    const v = virtuals.get(e.document.uri);
    if (v?.opened && proxy) proxy.sendNotification('textDocument/didClose',{textDocument:{uri:v.generated}});
    virtuals.delete(e.document.uri); if (v) reverse.delete(v.generated);
    connection.sendDiagnostics({uri:e.document.uri,diagnostics:[]});
  });
  connection.onDocumentFormatting(p => {
    const doc = documents.get(p.textDocument.uri); if (!doc) return [];
    const newText = format(doc.uri)(doc.getText());
    return newText ? [{range:{start:{line:0,character:0},end:doc.positionAt(doc.getText().length)},newText}] : [];
  });
  function mapLocation(location) {
    const uri = reverse.get(location.uri), v = virtuals.get(uri);
    if (!v) return location.uri?.startsWith(pathToFileURL(temp).href) ? null : location;
    const range = mapRange(v,location.range); return range ? {...location,uri,range} : null;
  }
  async function forward(method,p,token) {
    const v = virtuals.get(p.textDocument.uri);
    if (!v?.valid) return null;
    const position = mapPosition(v,p.position); if (!position) return null;
    if (!await ready || !proxy) return null;
    if (token.isCancellationRequested) throw new ResponseError(LSPErrorCodes.RequestCancelled,'Cancelled');
    const result = await proxy.sendRequest(method,{...p,textDocument:{uri:v.generated},position},token);
    if (virtuals.get(v.uri) !== v) throw new ResponseError(LSPErrorCodes.ContentModified,'Buffer changed');
    if (method === 'textDocument/hover') {
      if (!result?.range) return result;
      const range = mapRange(v,result.range); return range ? {...result,range} : null;
    }
    if (method === 'textDocument/definition') {
      return (Array.isArray(result) ? result : result ? [result] : []).map(l => {
        if (l.targetUri) {
          const target = mapLocation({uri:l.targetUri,range:l.targetRange});
          const selection = mapLocation({uri:l.targetUri,range:l.targetSelectionRange});
          const origin = l.originSelectionRange && mapRange(v,l.originSelectionRange);
          return target && selection ? {...l,targetUri:target.uri,targetRange:target.range,targetSelectionRange:selection.range,originSelectionRange:origin || undefined} : null;
        }
        return mapLocation(l);
      }).filter(Boolean);
    }
    if (method === 'textDocument/completion') {
      const items = (Array.isArray(result) ? result : result?.items || []).flatMap(item => {
        const mapEdit = edit => {
          if (edit.range) {const range = mapEditRange(v,edit.range); return range ? {...edit,range} : null;}
          const insert = mapEditRange(v,edit.insert), replace = mapEditRange(v,edit.replace);
          return insert && replace ? {...edit,insert,replace} : null;
        };
        const textEdit = item.textEdit && mapEdit(item.textEdit);
        const additionalTextEdits = item.additionalTextEdits?.map(mapEdit);
        if (item.textEdit && !textEdit || additionalTextEdits?.some(e => !e)) return [];
        return [{...item,textEdit,additionalTextEdits}];
      });
      return {isIncomplete:result?.isIncomplete || false,items};
    }
    return result;
  }
  connection.onHover((p,t) => forward('textDocument/hover',p,t));
  connection.onCompletion(async (p,t) => {
    const doc = documents.get(p.textDocument.uri); if (!doc) return {isIncomplete:false,items:[]};
    const prefix = doc.getText().slice(0,doc.offsetAt(p.position));
    let labels;
    if (/<\/?[\w-]*$/.test(prefix)) labels = html.elements;
    else if (/\bclass\s*=\s*"[^"]*$/.test(prefix)) {
      labels = [...new Set([...stylesAt(doc.uri)(doc.getText())(p.position).matchAll(/\.([\w-]+)/g)].map(m => m[1]))];
    } else if (/<[a-z][\w:-]*(?:\s+[^<>]*)?\s+[\w:-]*$/.test(prefix)) labels = html.attributes;
    if (labels) return {isIncomplete:false,items:labels.map(label => ({label,kind:10}))};
    return await forward('textDocument/completion',p,t) || {isIncomplete:false,items:[]};
  });
  connection.onDefinition((p,t) => {
    const doc = documents.get(p.textDocument.uri);
    if (doc?.uri.endsWith('.psxt')) {
      const text = doc.getText(), offset = doc.offsetAt(p.position);
      for (const template of text.matchAll(/<template\s+head="([^"]+)"/g)) {
        if (offset >= template.index && offset <= template.index+template[0].length) {
          const name = template[1].split(/\s/)[0];
          return [...text.matchAll(/foreign\s+import\s+(\w+)\s*::/g)].filter(m => m[1] === name).map(m => ({uri:doc.uri,range:{start:doc.positionAt(m.index),end:doc.positionAt(m.index+m[0].length)}}));
        }
      }
      for (const m of text.matchAll(/foreign\s+import\s+(\w+)\s*::/g)) {
        if (offset >= m.index && offset <= m.index+m[0].length) {
          const targets = [...text.matchAll(/<template\s+head="([^"]+)"/g)].filter(t => t[1].split(/\s/)[0] === m[1]);
          return targets.map(t => ({uri:doc.uri,range:{start:doc.positionAt(t.index),end:doc.positionAt(t.index+t[0].length)}}));
        }
      }
    }
    return forward('textDocument/definition',p,t);
  });
  connection.onShutdown(async () => {shuttingDown=true; if (proxy) {try {await Promise.race([proxy.sendRequest('shutdown'),new Promise(resolve => {const timer=setTimeout(resolve,3000);timer.unref();})]);} catch { /* child already stopped */ }}});
  connection.onExit(() => {cleanup();process.exit(shuttingDown ? 0 : 1);});
  documents.listen(connection); connection.listen();
};
