import { mkdirSync, writeFileSync } from 'node:fs';
import { build } from 'esbuild';
for (const name of ['Counter', 'HelloWorld']) {
  mkdirSync('examples/react/dist', { recursive: true });
  await build({ stdin: { contents: `import { main } from './output/React.Examples.${name}/index.js'; main();`, resolveDir: process.cwd() }, bundle: true, outfile: `examples/react/dist/${name}.js` });
  writeFileSync(`examples/react/dist/${name}.html`, `<div id="root"></div><script src="${name}.js"></script>`);
}
