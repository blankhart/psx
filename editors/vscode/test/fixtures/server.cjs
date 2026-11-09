// A real stdio peer for extension-host lifecycle/configuration tests.
const fs = require('node:fs');
const { createConnection } = require('vscode-languageserver/node');
const connection = createConnection(process.stdin, process.stdout);
const log = (event, data) => fs.appendFileSync(process.env.PSX_TEST_LOG, JSON.stringify({event, pid:process.pid, cwd:process.cwd(), data})+'\n');
connection.onInitialize(params => {
  log('initialize', params);
  return {capabilities:{textDocumentSync:1, documentFormattingProvider:true}};
});
connection.onDidOpenTextDocument(params => log('open', params));
connection.onDocumentFormatting(() => []);
connection.onShutdown(() => log('shutdown'));
connection.onExit(() => {log('exit'); process.exit(0);});
connection.listen();
