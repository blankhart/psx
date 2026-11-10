"""Zed manifest, native grammar/query and injection gate; no LSP needed."""
import ctypes
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import tomllib

from tree_sitter import Language, Parser, Query, QueryCursor
import tree_sitter_css

ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parents[1]
GRAMMAR = REPO / 'editors/tree-sitter-psx'
manifest = tomllib.loads((ROOT / 'extension.toml').read_text())
assert manifest['schema_version'] == 1 and manifest['id'] == 'psx'
assert manifest['languages'] == ['languages/psx', 'languages/psxt']
# Resolve the exact pinned source locally, independent of the current checkout.
for name, entry in manifest['grammars'].items():
    assert entry['repository'] == 'https://github.com/blankhart/psx'
    assert re.fullmatch('[0-9a-f]{40}', entry['rev'])
    for relative in ['src/parser.c', 'src/scanner.c']:
        pinned = subprocess.check_output(['git', 'show', f"{entry['rev']}:{entry['path']}/{relative}"], cwd=REPO)
        assert pinned == (REPO / entry['path'] / relative).read_bytes(), 'Update grammar pin deliberately'

settings = json.loads((ROOT / 'test/fixtures/settings.json').read_text())
assert len(settings['lsp']['psx-language-server']['binary']['arguments']) == 2
capsule = ctypes.pythonapi.PyCapsule_New
capsule.restype = ctypes.py_object
capsule.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_void_p]
libraries = []
with tempfile.TemporaryDirectory(prefix='psx zed ') as temp:
    langs = {}
    for name in ['psx', 'psxt']:
        source = GRAMMAR if name == 'psx' else GRAMMAR / name
        sdk = os.environ.get('WASI_SDK_PATH')
        if sdk:
            wasm = Path(temp) / f'{name}.wasm'
            subprocess.run([str(Path(sdk) / 'bin/clang'), '-fPIC', '-shared', '-Os',
                            f'-Wl,--export=tree_sitter_{name}', '-Wl,--max-memory=4294967296',
                            '-o', str(wasm), '-I', str(source / 'src'),
                            str(source / 'src/parser.c'), str(source / 'src/scanner.c')], check=True)
            assert wasm.read_bytes()[:8] == b'\x00asm\x01\x00\x00\x00'
            print(f'Built Zed {name}.wasm with WASI SDK {Path(sdk).name}')
        library = Path(temp) / f'{name}.so'
        # Exactly Zed's include path; scanner relative includes must work too.
        subprocess.run(['cc', '-shared', '-fPIC', '-Os', '-I', str(source / 'src'),
                        str(source / 'src/parser.c'), str(source / 'src/scanner.c'),
                        '-o', str(library)], check=True)
        dll = ctypes.CDLL(str(library)); libraries.append(dll)
        fn = getattr(dll, f'tree_sitter_{name}'); fn.restype = ctypes.c_void_p
        langs[name] = Language(capsule(fn(), b'tree_sitter.Language', None))
    for name, lang in langs.items():
        directory = ROOT / 'languages' / name
        config = tomllib.loads((directory / 'config.toml').read_text())
        assert config['name'] == name.upper() and config['grammar'] == name
        assert config['path_suffixes'] == [name]
        assert config['tab_size'] == 2 and config['hard_tabs'] is False
        query_source = GRAMMAR if name == 'psx' else GRAMMAR / name
        for kind in ['highlights', 'injections', 'brackets', 'indents']:
            assert (directory / f'{kind}.scm').read_bytes() == (query_source / 'queries' / f'{kind}.scm').read_bytes(), 'Refresh shared queries'
        queries = {p.stem: Query(lang, p.read_text()) for p in directory.glob('*.scm')}
        assert set(queries) == {'highlights', 'injections', 'brackets', 'indents', 'overrides'}
        source = (ROOT / f'test/fixtures/example.{name}').read_bytes()
        tree = Parser(lang).parse(source)
        assert not tree.root_node.has_error, str(tree.root_node)
        captures = QueryCursor(queries['highlights']).captures(tree.root_node)
        assert captures['tag'] and captures['variable']
        assert QueryCursor(queries['brackets']).captures(tree.root_node)['open']
        assert QueryCursor(queries['indents']).captures(tree.root_node)['indent']
        assert QueryCursor(queries['overrides']).captures(tree.root_node)['purescript']
        languages = set()
        for pattern, match in QueryCursor(queries['injections']).matches(tree.root_node):
            injected = queries['injections'].pattern_settings(pattern)['injection.language']
            languages.add(injected)
            parser = Parser(langs['psx'] if injected == 'psx' else Language(tree_sitter_css.language()))
            for node in match['injection.content']:
                result = parser.parse(node.text)
                assert not result.root_node.has_error, (injected, str(result.root_node))
                if injected == 'psx':
                    q = Query(langs['psx'], (ROOT / 'languages/psx/highlights.scm').read_text())
                    assert QueryCursor(q).captures(result.root_node)['keyword']
        assert languages == ({'css', 'psx'} if name == 'psxt' else {'css'})
print('Zed manifest, pinned grammar sources, both languages, all queries and CSS/PSX injections passed (server disabled)')
