const path = require('node:path');
const {runTests} = require('@vscode/test-electron');
runTests({
  version:'1.105.1',
  extensionDevelopmentPath:[path.resolve(__dirname,'..'),path.join(__dirname,'grammar-dependency')],
  extensionTestsPath:path.resolve(__dirname,process.env.PSX_VSCODE_REAL ? 'real-host.cjs' : 'host.cjs'),
  launchArgs:['--no-sandbox','--disable-gpu','--disable-workspace-trust', path.resolve(__dirname,'test.code-workspace')],
  ...(process.env.VSCODE_EXECUTABLE_PATH ? {vscodeExecutablePath:process.env.VSCODE_EXECUTABLE_PATH} : {})
}).catch(error => {console.error(error);process.exitCode=1;});
