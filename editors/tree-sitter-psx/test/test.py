"""Native parser integration tests; runtime pinned in requirements.txt."""
import ctypes
import json
from pathlib import Path
import subprocess
import tempfile
import sys
import tree_sitter_css
from tree_sitter import Language, Parser, Query, QueryCursor

ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parents[1]
CAPSULE = ctypes.pythonapi.PyCapsule_New
CAPSULE.restype = ctypes.py_object
CAPSULE.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_void_p]
LIBRARIES = []

def language(name, out):
    directory = ROOT if name == 'psx' else ROOT / 'psxt'
    lib = out / f'{name}.so'
    subprocess.run(['cc', '-shared', '-fPIC', '-std=c11', '-D_DEFAULT_SOURCE', '-O1',
                    '-I', str(directory / 'src'), '-I', str(ROOT),
                    str(directory / 'src/parser.c'), str(directory / 'src/scanner.c'),
                    '-o', str(lib)], check=True)
    dll = ctypes.CDLL(str(lib)); LIBRARIES.append(dll)
    fn = getattr(dll, f'tree_sitter_{name}'); fn.restype = ctypes.c_void_p
    return Language(CAPSULE(fn(), b'tree_sitter.Language', None))

def nodes(node):
    yield node
    for child in node.children:
        yield from nodes(child)

def errors(tree):
    found = [n for n in nodes(tree.root_node) if n.is_error or n.is_missing or n.type == 'unclosed_tag']
    # Missing hidden tokens can set has_error without appearing as children.
    return found or ([tree.root_node] if tree.root_node.has_error else [])

with tempfile.TemporaryDirectory() as directory:
    langs = {name: language(name, Path(directory)) for name in ['psx', 'psxt']}
    parsers = {name: Parser(lang) for name, lang in langs.items()}
    fixtures = json.loads((ROOT / 'test/fixtures.json').read_text())
    # Compiling every editor query catches stale or unreachable node names.
    queries = {}
    for name, lang in langs.items():
        base = ROOT if name == 'psx' else ROOT / 'psxt'
        queries[name] = {p.stem: Query(lang, p.read_text()) for p in (base / 'queries').glob('*.scm')}
    listed = fixtures['valid'] + fixtures['invalid']
    actual = {str(p.relative_to(REPO / 'test/corpus')) for p in (REPO / 'test/corpus').rglob('input.*') if p.suffix in ['.psx', '.psxt']}
    assert len(listed) == len(set(listed)) and set(listed) == actual, 'Update the fixture manifest for corpus changes'
    failed = []
    for relative in fixtures['valid']:
        path = REPO / 'test/corpus' / relative
        source = path.read_bytes(); parser = parsers[path.suffix[1:]]
        tree = parser.parse(source)
        if errors(tree):
            failed.append(relative)
            print(relative)
            for n in errors(tree)[:4]: print(' ', n.type, n.start_point, repr(source[n.start_byte:n.end_byte][:100]))
    assert not failed, f'{len(failed)} valid fixtures failed'
    print(f"Parsed {len(fixtures['valid'])} valid corpus inputs")

    for name, sources in json.loads((ROOT / 'test/cases.json').read_text()).items():
        for source in sources:
            tree = parsers[name].parse(source.encode())
            assert not errors(tree), (name, source, str(tree.root_node))

    # Invalid parser and PSXT semantic fixtures have explicit recovery goldens.
    recovery = {}
    for relative in fixtures['invalid']:
        path = REPO / 'test/corpus' / relative
        tree = parsers[path.suffix[1:]].parse(path.read_bytes())
        recovery[relative] = str(tree.root_node)
        if relative in ['parse/error-hole/input.psx', 'parse/error-trait-hole/input.psx', 'parse/error-unclosed/input.psx']:
            assert errors(tree), relative
            assert any(n.type == 'function' and n.child_by_field_name('name') is not None
                       and n.child_by_field_name('name').text == b'next' for n in nodes(tree.root_node)), relative
    golden = ROOT / 'test/recovery.json'
    if '--accept' in sys.argv:
        golden.write_text(json.dumps(recovery, indent=2) + '\n')
    assert recovery == json.loads(golden.read_text()), 'Recovery trees changed; review --accept output'

    def captures(name, kind, source):
        tree = parsers[name].parse(source)
        assert not errors(tree), tree.root_node
        return QueryCursor(queries[name][kind]).captures(tree.root_node)

    def has_capture(caps, name, text):
        assert any(n.text == text for n in caps.get(name, [])), (name, text, caps)

    src = b'module M where\nf a b = a <b\ng = (<> )\nh = <p class="x">{if a then <b/> else <i/>}</p>\n'
    caps = captures('psx', 'highlights', src)
    has_capture(caps, 'operator', b'<')
    has_capture(caps, 'operator', b'<>')
    has_capture(caps, 'tag', b'<p')
    has_capture(caps, 'attribute', b'class')
    assert len(caps['tag']) == 4
    src = b'<script type="text/purescript">module M where\nx = <p/>\n</script><template head="row { id, label } | id > 0"><p>{label}</p><style>p { color: red; }</style></template>'
    caps = captures('psxt', 'highlights', src)
    has_capture(caps, 'function', b'row')
    has_capture(caps, 'variable', b'label')
    has_capture(caps, 'operator', b'>')
    injection_tree = parsers['psxt'].parse(src)
    matches = QueryCursor(queries['psxt']['injections']).matches(injection_tree.root_node)
    languages = []
    for pattern, cap in matches:
        injected = cap['injection.content'][0].text
        lang = queries['psxt']['injections'].pattern_settings(pattern)['injection.language']
        languages.append(lang)
        parser = parsers['psx'] if lang == 'psx' else Parser(Language(tree_sitter_css.language()))
        tree = parser.parse(injected)
        assert not errors(tree), (lang, tree.root_node)
        if lang == 'css':
            css_query = Query(parser.language, '(property_name) @property (plain_value) @value')
            caps = QueryCursor(css_query).captures(tree.root_node)
            has_capture(caps, 'property', b'color'); has_capture(caps, 'value', b'red')
        else:
            has_capture(captures('psx', 'highlights', injected), 'tag', b'<p')
    assert sorted(languages) == ['css', 'psx']
    for name, source in [('psx', b'module M where\nf = <p>{x}</p>\n'),
                         ('psxt', b'<template head="f"><p>{x}</p></template>')]:
        for kind in ['brackets', 'indents', 'folds']:
            assert captures(name, kind, source), (name, kind)
    assert not QueryCursor(queries['psxt']['injections']).matches(
        parsers['psxt'].parse(b'<script type="text/javascript">let x = 1</script>').root_node)

    # Every valid script body is independently parsed by the injected PSX grammar.
    for relative in fixtures['valid']:
        if not relative.endswith('.psxt'): continue
        source = (REPO / 'test/corpus' / relative).read_bytes()
        tree = parsers['psxt'].parse(source)
        for pattern, cap in QueryCursor(queries['psxt']['injections']).matches(tree.root_node):
            if queries['psxt']['injections'].pattern_settings(pattern)['injection.language'] == 'psx':
                assert not errors(parsers['psx'].parse(cap['injection.content'][0].text)), relative

    def point(source, offset):
        prefix = source[:offset]
        return (prefix.count(b'\n'), len(prefix.rsplit(b'\n', 1)[-1]))

    def edit(name, source, old, new, broken):
        parser = parsers[name]
        tree = parser.parse(source)
        assert not errors(tree)
        start = source.index(old)
        changed = source[:start] + new + source[start + len(old):]
        tree.edit(start, start + len(old), start + len(new),
                  point(source, start), point(source, start + len(old)), point(changed, start + len(new)))
        incremental = parser.parse(changed, tree)
        fresh = parser.parse(changed)
        assert str(incremental.root_node) == str(fresh.root_node), (name, old, new)
        if broken: assert errors(incremental), (name, changed, str(incremental.root_node))
        # Fix the edit using the damaged tree, not a fresh reparse.
        incremental.edit(start, start + len(new), start + len(old),
                         point(changed, start), point(changed, start + len(new)), point(source, start + len(old)))
        fixed = parser.parse(source, incremental)
        assert not errors(fixed), (name, fixed.root_node)
        assert str(fixed.root_node) == str(parser.parse(source).root_node)

    for name, prefix, suffix in [('psx', b'module M where\nx = ', b'\nafter = 1\n'),
                                 ('psxt', b'<template head="view x">', b'</template>')]:
        source = prefix + '<div><p>{show "🎉"}</p><br/></div>'.encode() + suffix
        for old, new in [(b'</p>', b'</'), (b'<p>', b'<p'), ('{show "🎉"}'.encode(), b'{}'), (b'}</p>', b'</p>')]:
            edit(name, source, old, new, True)
        edit(name, source, '🎉'.encode(), 'café'.encode(), False)
        edit(name, source, b'<br/>', b'<hr/>', False)
    edit('psxt', src, b'color: red', b'color: blue', False)
    edit('psxt', src, b'row { id, label }', b'row { id, label', True)
    edit('psxt', src, b'</style>', b'</style', True)
    print(f"Recovery, queries, injections and incremental edits passed ({len(fixtures['invalid'])} error fixtures)")
