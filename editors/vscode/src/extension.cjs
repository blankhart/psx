const vscode = require('vscode');
const path = require('node:path');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let shutdown = async () => {};
function activate(context) {
  const output = vscode.window.createOutputChannel('PSX');
  const clients = new Map();
  const warned = new Set();
  let queue = Promise.resolve(), disposed = false;
  const enqueue = task => (queue = queue.then(task).catch(error => {
    output.appendLine(String(error.stack || error));
    void vscode.window.showErrorMessage(`PSX: ${error.message}. See the PSX output log.`, 'Show Output').then(action => {
      if (action) output.show();
    });
  }));
  const stop = async () => {
    const running = [...clients.values()];
    clients.clear();
    await Promise.all(running.map(client => client.stop()));
  };
  async function start(folder) {
    if(disposed || clients.has(folder.uri.toString())) return;
    const config = vscode.workspace.getConfiguration('psx',folder.uri);
    if(!config.get('server.enabled')) return;
    const command = config.get('server.command');
    if(!command) {
      if(!warned.has(folder.uri.toString())) {
        warned.add(folder.uri.toString());
        output.appendLine(`${folder.name}: set psx.server.command and psx.server.args to the built PSX stdio server. Highlighting remains available.`);
        void vscode.window.showWarningMessage('PSX server is not configured. Set psx.server.command and psx.server.args; see the extension README.', 'Open Settings').then(action => {
          if(action) void vscode.commands.executeCommand('workbench.action.openSettings','psx.server');
        });
      }
      return;
    }
    const root = config.get('projectRoot') || folder.uri.fsPath;
    if(!path.isAbsolute(root)) throw Error('psx.projectRoot must be an absolute path');
    const options = {command, args:config.get('server.args'), transport:TransportKind.stdio,
      options:{cwd:root, env:{...process.env, ELECTRON_RUN_AS_NODE:'1'}}};
    const client = new LanguageClient('psx',`PSX (${folder.name})`,options,{
      workspaceFolder:folder,
      documentSelector:['psx','psxt'].map(language=>({scheme:'file',language,
        pattern:new vscode.RelativePattern(folder,'**/*')})),
      outputChannel:output,
      initializationOptions:{projectRoot:root,
        ...(config.get('purescriptLanguageServer') ? {purescriptLanguageServer:config.get('purescriptLanguageServer')} : {}),
        purescript:config.get('purescript')},
      initializationFailedHandler:() => false
    });
    clients.set(folder.uri.toString(),client);
    try {await client.start();} catch(error) {
      clients.delete(folder.uri.toString());
      await client.dispose();
      throw Error(`Could not start PSX for ${folder.name}: ${error.message}. Check the executable, argument array, project root and server build`);
    }
  }
  async function reconcile(restart=false) {
    if(disposed) return;
    if(restart) await stop();
    const folders = vscode.workspace.workspaceFolders || [];
    for(const [uri,client] of clients) if(!folders.some(folder=>folder.uri.toString()===uri)) {
      clients.delete(uri);await client.stop();
    }
    for(const document of vscode.workspace.textDocuments) {
      if(!['psx','psxt'].includes(document.languageId) || document.uri.scheme!=='file') continue;
      const folder = vscode.workspace.getWorkspaceFolder(document.uri);
      if(folder) await start(folder);
      else if(!warned.has(document.uri.toString())) {
        warned.add(document.uri.toString());
        output.appendLine('Open a workspace folder for PSX language features. Standalone files have highlighting only.');
      }
    }
  }
  context.subscriptions.push(output,
    vscode.commands.registerCommand('psx.restartServer',()=>enqueue(()=>reconcile(true))),
    vscode.commands.registerCommand('psx.showOutput',()=>output.show()),
    vscode.workspace.onDidOpenTextDocument(document=>{
      if(['psx','psxt'].includes(document.languageId)) void enqueue(()=>reconcile());
    }),
    vscode.workspace.onDidChangeWorkspaceFolders(()=>void enqueue(()=>reconcile())),
    vscode.workspace.onDidChangeConfiguration(event=>{
      if(event.affectsConfiguration('psx')) void enqueue(()=>reconcile(true));
    }),
    {dispose:()=>{disposed=true;void enqueue(stop);}});
  shutdown = () => {disposed=true;return enqueue(stop);};
  void enqueue(()=>reconcile());
  return {stop:shutdown};
}
async function deactivate() { await shutdown(); }
module.exports = {activate,deactivate};
