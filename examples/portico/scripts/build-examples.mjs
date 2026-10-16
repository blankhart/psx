// Bundles every example in src/ into examples/dist/<Name>.{js,html,css}, where
// Portico's smoke test (scripts/smoke-test.mjs, a verbatim copy of Portico's
// own) looks for them. Run `psx build examples/portico/src` and `spago build`
// first; this only bundles what the compiler left in output/.
//
// Adapted from Portico's scripts/build-examples.mjs: the modules come from
// .psx sources as well as .purs ones, and the compiler output is the
// workspace's.
//
//   node examples/portico/scripts/build-examples.mjs          build
//   node examples/portico/scripts/build-examples.mjs clean    remove examples/dist

import { copyFileSync, existsSync, mkdirSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { build } from 'esbuild';

const root = fileURLToPath(new URL('..', import.meta.url));
const workspace = fileURLToPath(new URL('../../..', import.meta.url));
const src = root + 'src/';
const dist = root + 'examples/dist/';

if (process.argv[2] === 'clean') {
  rmSync(dist, { recursive: true, force: true });
  process.exit(0);
}

const names = [
  ...new Set(
    readdirSync(src)
      .filter((file) => file.endsWith('.purs') || file.endsWith('.psx'))
      .map((file) => file.replace(/\.(purs|psx)$/, ''))
  ),
].sort();

mkdirSync(dist, { recursive: true });

for (const name of names) {
  const module = `${workspace}output/Examples.${name}/index.js`;
  if (!existsSync(module)) {
    throw new Error(`Examples.${name} has not been compiled. Run \`psx build\` and \`spago build\` first.`);
  }

  await build({
    stdin: {
      contents: `import { main } from ${JSON.stringify(module)};\nmain();\n`,
      resolveDir: workspace,
    },
    bundle: true,
    minify: true,
    // Portico.Devtools labels messages by their constructors' names.
    keepNames: true,
    platform: 'browser',
    format: 'iife',
    outfile: `${dist}${name}.js`,
  });

  const hasCss = existsSync(`${src}${name}.css`);
  if (hasCss) copyFileSync(`${src}${name}.css`, `${dist}${name}.css`);

  writeFileSync(
    `${dist}${name}.html`,
    `<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>${name}</title>
    ${hasCss ? `<link rel="stylesheet" href="${name}.css">` : ''}
  </head>
  <body>
    <main></main>
    <script src="${name}.js"></script>
  </body>
</html>
`
  );
}

writeFileSync(
  `${dist}index.html`,
  `<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Portico examples in PSX</title>
  </head>
  <body>
    <ul>
${names.map((name) => `      <li><a href="${name}.html">${name}</a></li>`).join('\n')}
    </ul>
  </body>
</html>
`
);

console.log(`Built ${names.length} examples into examples/portico/examples/dist`);
