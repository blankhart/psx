"""Build checkout grammars into Zed's dev-extension cache using its WASI SDK."""
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent
GRAMMAR = ROOT.parent / 'tree-sitter-psx'


def build(name, source, output, sdk):
    subprocess.run([str(Path(sdk) / 'bin/clang'), '-fPIC', '-shared', '-Os',
                    f'-Wl,--export=tree_sitter_{name}', '-Wl,--max-memory=4294967296',
                    '-o', str(output), '-I', str(source / 'src'),
                    str(source / 'src/parser.c'), str(source / 'src/scanner.c')], check=True)
    # Successful linking permits unresolved imports. Check those too: Zed's
    # Tree-sitter runtime cannot instantiate arbitrary libc calls (e.g. strcpy).
    subprocess.run(['node', str(ROOT / 'test/wasm-imports.cjs'), str(output)], check=True)


if __name__ == '__main__':
    sdk = os.environ.get('WASI_SDK_PATH')
    if not sdk:
        raise SystemExit('Set WASI_SDK_PATH to the WASI SDK 34.0 directory (see README.md).')
    output = ROOT / 'grammars'
    if not all((output / name / '.git').is_dir() for name in ['psx', 'psxt']):
        raise SystemExit('Install the dev extension in Zed first to populate its grammar cache.')
    for name in ['psx', 'psxt']:
        source = GRAMMAR if name == 'psx' else GRAMMAR / name
        with tempfile.TemporaryDirectory(dir=output) as temp:
            wasm = Path(temp) / f'{name}.wasm'
            build(name, source, wasm, sdk)
            wasm.replace(output / f'{name}.wasm')
    print('Built checkout grammars. Restart Zed to reload PSX and PSXT.')
