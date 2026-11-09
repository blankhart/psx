const fs=require('node:fs');
const path=require('node:path');
const root=path.join(__dirname,'workspace with spaces');
for(const name of ['project one','project two']) {
  const dir=path.join(root,name);fs.rmSync(dir,{recursive:true,force:true});fs.mkdirSync(dir,{recursive:true});
  fs.copyFileSync(path.join(__dirname,'fixtures/server.cjs'),path.join(dir,'server with spaces.cjs'));
  for(const ext of ['psx','psxt']) fs.copyFileSync(path.join(__dirname,`fixtures/example.${ext}`),path.join(dir,`example.${ext}`));
}
fs.writeFileSync(path.join(__dirname,'test.code-workspace'),JSON.stringify({folders:['project one','project two'].map(name=>({path:path.join(root,name)}))}));
// Offline grammar-only dependency using the same pinned upstream grammar as
// the TextMate tests. It contains no client code and is never packaged.
const dependency=path.join(__dirname,'grammar-dependency');
fs.mkdirSync(path.join(dependency,'syntaxes'),{recursive:true});
fs.copyFileSync(path.join(__dirname,'../../textmate/test/vendor/purescript.json'),path.join(dependency,'syntaxes/purescript.json'));
fs.writeFileSync(path.join(dependency,'package.json'),JSON.stringify({name:'language-purescript',publisher:'nwolverson',version:'0.2.10',engines:{vscode:'^1.85.0'},contributes:{languages:[{id:'purescript',extensions:['.purs']}],grammars:[{language:'purescript',scopeName:'source.purescript',path:'syntaxes/purescript.json'}]}}));
