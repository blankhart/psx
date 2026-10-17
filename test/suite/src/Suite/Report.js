import { spawnSync } from 'node:child_process';
import { rmSync } from 'node:fs';

export const timeCommand = (cwd) => (command) => (args) => () => {
  const start = performance.now();
  const result = spawnSync(command, args, { cwd, encoding: 'utf8' });
  const seconds = (performance.now() - start) / 1000;
  return {
    ok: result.status === 0,
    seconds,
    output: `${result.stdout ?? ''}${result.stderr ?? ''}${result.error ? String(result.error) : ''}`,
  };
};

export const removeDir = (dir) => () => rmSync(dir, { recursive: true, force: true });

export const runStamp = () => ({
  commit: process.env.GITHUB_SHA ?? '',
  date: new Date().toISOString(),
  run: process.env.GITHUB_RUN_ID ?? '',
});
