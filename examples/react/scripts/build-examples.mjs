import { mkdirSync, writeFileSync } from 'node:fs';
import { build } from 'esbuild';
const names = ['Counter', 'HelloWorld'];
for (const name of names) {
  mkdirSync('examples/react/dist', { recursive: true });
  await build({ stdin: { contents: `import { main } from './output/React.Examples.${name}/index.js'; main();`, resolveDir: process.cwd() }, bundle: true, outfile: `examples/react/dist/${name}.js` });
  writeFileSync(`examples/react/dist/${name}.html`, `<div id="root"></div><script src="${name}.js"></script>`);
}

writeFileSync('examples/react/dist/index.html', `<!doctype html>
<html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width">
<title>PSX examples</title>
<style>body { font: 18px system-ui; max-width: 48rem; margin: 4rem auto; padding: 0 1rem; } li { margin: 1rem 0; }</style>
<h1>PSX examples</h1><p>React components written with PSX.</p>
<ul>${names.map(name => `<li><a href="${name}.html">${name}</a></li>`).join('')}</ul></html>`);
