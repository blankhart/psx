import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
const root = new URL('./', import.meta.url);
const executable = fileURLToPath(new URL('../../node_modules/.bin/tree-sitter', root));
for (const directory of ['./', './psxt/']) {
  const result = spawnSync(executable, ['generate', '--abi', '15'], {
    cwd: fileURLToPath(new URL(directory, root)), stdio: 'inherit',
  });
  if (result.error) throw result.error;
  if (result.status !== 0) process.exit(result.status ?? 1);
}
