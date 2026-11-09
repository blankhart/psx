const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const vscode = require('vscode');
const wait = async predicate => {
  for(let i=0;i<200;i++) {if(await predicate()) return; await new Promise(r=>setTimeout(r,100));}
  throw Error('Timed out waiting for extension/server');
};
exports.run = async () => {
  const extension = vscode.extensions.getExtension('psx.psx-vscode');
  assert.ok(extension);
  const contract = require('../../textmate/extension-contributions.json');
  assert.deepEqual(extension.packageJSON.contributes.languages,contract.contributes.languages);
  const grammars = extension.packageJSON.contributes.grammars;
  assert.deepEqual(grammars,contract.contributes.grammars);
  assert.deepEqual(grammars.map(g=>g.scopeName), ['source.psx','text.html.psxt']);
  for(const grammar of grammars) assert.ok(fs.existsSync(path.join(extension.extensionPath,grammar.path)));
  const temp = fs.mkdtempSync(path.join(os.tmpdir(),'psx extension test '));
  const log = path.join(temp,'events.jsonl');
  process.env.PSX_TEST_LOG = log;
  const events = () => fs.existsSync(log) ? fs.readFileSync(log,'utf8').trim().split('\n').filter(Boolean).map(JSON.parse) : [];
  try {
    for (const folder of vscode.workspace.workspaceFolders) {
      const config = vscode.workspace.getConfiguration('psx',folder.uri);
      await config.update('server.command',process.execPath,vscode.ConfigurationTarget.WorkspaceFolder);
      await config.update('server.args',[path.join(folder.uri.fsPath,'server with spaces.cjs'),'--stdio'],vscode.ConfigurationTarget.WorkspaceFolder);
      await config.update('projectRoot',folder.uri.fsPath,vscode.ConfigurationTarget.WorkspaceFolder);
      await config.update('purescriptLanguageServer','/compiler path with spaces/pls',vscode.ConfigurationTarget.WorkspaceFolder);
      await config.update('purescript',{pursExe:'/compiler path/purs',outputDirectory:'custom output'},vscode.ConfigurationTarget.WorkspaceFolder);
    }
    for (const folder of vscode.workspace.workspaceFolders) {
      for (const ext of ['psx','psxt']) {
        const doc = await vscode.workspace.openTextDocument(vscode.Uri.joinPath(folder.uri,`example.${ext}`));
        assert.equal(doc.languageId,ext);
        await vscode.window.showTextDocument(doc);
      }
    }
    await extension.activate();
    await wait(()=>events().filter(e=>e.event==='open').length>=4);
    const inits = events().filter(e=>e.event==='initialize');
    assert.equal(inits.length,2);
    for(const e of inits) {
      assert.equal(e.data.initializationOptions.projectRoot,e.cwd);
      assert.equal(e.data.initializationOptions.purescript.pursExe,'/compiler path/purs');
      assert.equal(e.data.initializationOptions.purescriptLanguageServer,'/compiler path with spaces/pls');
      assert.equal(e.data.workspaceFolders.length,1);
    }
    const old = inits.map(e=>e.pid);
    await vscode.commands.executeCommand('psx.restartServer');
    await wait(()=>events().filter(e=>e.event==='initialize').length===4);
    await wait(()=>old.every(pid=>events().some(e=>e.pid===pid && e.event==='exit')));
    await vscode.commands.executeCommand('psx.showOutput');
    await extension.exports.stop();
    await wait(()=>events().filter(e=>e.event==='exit').length===4);
    console.log('VS Code language activation, multi-root forwarding, restart and shutdown passed');
  } catch(error) {
    console.log(JSON.stringify(events().map(e=>({event:e.event,pid:e.pid}))));
    throw error;
  } finally {
    await extension.exports?.stop();
    fs.rmSync(temp,{recursive:true,force:true});
  }
};
