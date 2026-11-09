const assert = require('node:assert/strict');
const path = require('node:path');
const fs = require('node:fs');
const vscode = require('vscode');
const wait = async predicate => {
  for(let i=0;i<240;i++) {if(await predicate()) return;await new Promise(r=>setTimeout(r,250));}
  throw Error('Timed out waiting for real PSX feature');
};
exports.run = async () => {
  const timeout=setTimeout(()=>{console.error('Real server test timed out');process.exit(1);},120000);
  const extension=vscode.extensions.getExtension('psx.psx-vscode');
  const folder=vscode.workspace.workspaceFolders[0];
  const root=path.resolve(__dirname,'../../..');
  const config=vscode.workspace.getConfiguration('psx',folder.uri);
  await config.update('server.command',process.execPath,vscode.ConfigurationTarget.WorkspaceFolder);
  await config.update('server.args',[path.join(root,'packages/psx-language-server/bin/psx-language-server.mjs'),'--stdio'],vscode.ConfigurationTarget.WorkspaceFolder);
  await config.update('projectRoot',root,vscode.ConfigurationTarget.WorkspaceFolder);
  await config.update('purescriptLanguageServer','',vscode.ConfigurationTarget.WorkspaceFolder);
  await config.update('purescript',{pursExe:path.join(root,'node_modules/.bin/purs'),outputDirectory:'output'},vscode.ConfigurationTarget.WorkspaceFolder);
  try {
    for(const ext of ['psx','psxt']) {
      const script='module VscodeReal where\n\nimport Prelude\nimport Psx.Static\n\nlabel :: String\nlabel = show unit\n';
      const text=ext==='psx' ? script+'view = <div>😀 {label}</div>\n' : '<script type="text/purescript">\n'+script+'foreign import view :: Markup Unit\n</script>\n<template head="view"><div>😀 {label}</div></template>\n';
      const uri=vscode.Uri.joinPath(folder.uri,`real.${ext}`);
      fs.writeFileSync(uri.fsPath,text);
      const doc=await vscode.workspace.openTextDocument(uri);
      await vscode.window.showTextDocument(doc);
      assert.equal(doc.languageId,ext);
      const lines=text.split('\n'), line=lines.findIndex(l=>l.includes('{label}'));
      const pos=new vscode.Position(line,lines[line].indexOf('{label}')+2);
      await wait(async()=> (await vscode.commands.executeCommand('vscode.executeHoverProvider',uri,pos))?.length);
      const completions=await vscode.commands.executeCommand('vscode.executeCompletionItemProvider',uri,pos.translate(0,1));
      assert.ok(completions.items.some(i=>(typeof i.label==='string' ? i.label : i.label.label).startsWith('label')));
      const definitions=await vscode.commands.executeCommand('vscode.executeDefinitionProvider',uri,pos);
      assert.ok(definitions.some(d=>(d.uri || d.targetUri).toString()===uri.toString() && (d.range || d.targetSelectionRange).start.line===lines.findIndex(l=>l.startsWith('label ='))));
      const edits=await vscode.commands.executeCommand('vscode.executeFormatDocumentProvider',uri,{tabSize:2,insertSpaces:true});
      assert.ok(edits.length);
      const change=new vscode.WorkspaceEdit();
      change.replace(uri,new vscode.Range(pos.translate(0,-1),pos.translate(0,4)),'label + 1');
      assert.ok(await vscode.workspace.applyEdit(change));
      await wait(()=>vscode.languages.getDiagnostics(uri).some(d=>d.source==='purescript' && d.range.start.line===line));
      console.log(`VS Code real server ${ext}: hover, completion, definition, formatting and mapped type diagnostic passed`);
    }
  } finally {clearTimeout(timeout);await extension.exports?.stop();}
};
