const fs = require('node:fs');
const path = require('node:path');
const esbuild = require('esbuild');
const root=__dirname;
const source=path.join(root,'../textmate');
fs.mkdirSync(path.join(root,'syntaxes'),{recursive:true});
for(const name of ['psx','psxt']) {
  fs.copyFileSync(path.join(source,'syntaxes',`${name}.tmLanguage.json`),path.join(root,'syntaxes',`${name}.tmLanguage.json`));
  fs.copyFileSync(path.join(source,`${name}-language-configuration.json`),path.join(root,`${name}-language-configuration.json`));
}
esbuild.buildSync({entryPoints:[path.join(root,'src/extension.cjs')],bundle:true,platform:'node',format:'cjs',target:'node20',external:['vscode'],outfile:path.join(root,'dist/extension.cjs')});
