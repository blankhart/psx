const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {spawn} = require('node:child_process');
const {once} = require('node:events');
const {createMessageConnection,StreamMessageReader,StreamMessageWriter} = require('vscode-jsonrpc/node');

test('fixture records a clean shutdown when EOF precedes the exit notification', {timeout:10000}, async () => {
  const dir=fs.mkdtempSync(path.join(os.tmpdir(),'psx fixture EOF '));
  const log=path.join(dir,'events.jsonl');
  const child=spawn(process.execPath,[path.join(__dirname,'fixtures/server.cjs')],{env:{...process.env,PSX_TEST_LOG:log}});
  const exited=once(child,'exit');
  const connection=createMessageConnection(new StreamMessageReader(child.stdout),new StreamMessageWriter(child.stdin));
  child.stderr.on('data',data=>process.stderr.write(data));
  connection.listen();
  try {
    await connection.sendRequest('initialize',{processId:null,capabilities:{}});
    await connection.sendRequest('shutdown');
    // Deliberately omit exit to reproduce the losing side of the dispatch race.
    child.stdin.end();
    const [code,signal]=await exited;
    assert.equal(code,0);assert.equal(signal,null);
    const events=fs.readFileSync(log,'utf8').trim().split('\n').map(JSON.parse);
    assert.deepEqual(events.map(e=>e.event),['initialize','shutdown','exit']);
    assert.equal(events.at(-1).data.code,0);
  } finally {
    connection.dispose();child.kill();fs.rmSync(dir,{recursive:true,force:true});
  }
});
