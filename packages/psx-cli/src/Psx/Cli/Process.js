import { spawnSync } from 'node:child_process';

// Runs a command to completion with the terminal's stdin, capturing its
// output.
export const runSync_ = function (command, args) {
  return function () {
    const result = spawnSync(command, args, {
      encoding: 'utf8',
      stdio: ['inherit', 'pipe', 'pipe'],
      maxBuffer: 1 << 30,
    });
    return {
      status: result.status === null ? (result.error ? 127 : 1) : result.status,
      stdout: result.stdout || '',
      stderr: result.stderr || (result.error ? String(result.error) : ''),
    };
  };
};
