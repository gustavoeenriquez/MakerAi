"""Aplica un mapa de excepciones: literal -> AiStr(clave), uses y tablas.

Uso: python exc_apply.py items.json map_module "Título de la sección"
     [--dry]
"""
import importlib, json, os, re, sys
HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(os.path.dirname(os.path.dirname(HERE)), 'Source')
items_path, map_mod, title = sys.argv[1], sys.argv[2], sys.argv[3]
# el módulo del mapa se busca en la carpeta actual (p. ej. el scratchpad)
sys.path.insert(0, os.getcwd())
dry = '--dry' in sys.argv
mp = importlib.import_module(map_mod)


def norm(lit):
    return ''.join(c if (32 <= ord(c) < 127 and c != '?') else '?' for c in lit)


NMAP = {norm(k): v for k, v in mp.M.items()}
NMANUAL = {norm(k) for k in mp.MANUAL_LITS}
DEFS = {}
for k, v in list(mp.M.items()) + [(None, v) for v in mp.MANUAL.values()]:
    key, en, es = v
    if en is not None:
        if key in DEFS and DEFS[key] != (en, es):
            raise SystemExit(f'clave {key} con dos textos distintos')
        DEFS[key] = (en, es)
for k, v in mp.M.items():
    if v[0] not in DEFS:
        raise SystemExit(f'clave {v[0]} sin texto')

items = json.load(open(items_path, encoding='utf-8'))
by_file = {}
missing = set()
for it in items:
    if it['file'].endswith('uMakerAi.Localization.pas'):
        continue
    n = norm(it['lit'])
    if n in NMANUAL:
        continue
    if n not in NMAP:
        missing.add(it['lit'])
        continue
    by_file.setdefault(it['file'], []).append(it)
if missing:
    print('SIN MAPA:')
    for m in sorted(missing):
        print('  ', m)
    raise SystemExit(1)

used_keys = set()
changed_files = []
texts = {}  # rel -> (texto modificado, bom)
for rel, its in sorted(by_file.items()):
    path = os.path.join(SRC, rel)
    raw = open(path, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig')
    lines = text.split('\n')
    for it in its:
        key = NMAP[norm(it['lit'])][0]
        used_keys.add(key)
        i = it['line'] - 1
        line = lines[i]
        lit = it['lit']
        if it['concat']:
            rx = re.compile(r'\.Create\(\s*' + re.escape(lit) + r'\s*\+\s*(.*)\);(\s*)$')
            m = rx.search(line)
            if not m:
                raise SystemExit(f'{rel}:{it["line"]}: concatenación no reconocida: {line.strip()}')
            line = line[:m.start()] + f".CreateFmt(AiStr('{key}'), [{m.group(1)}]);" + m.group(2)
        else:
            rx = re.compile(r'(\.Create(?:Fmt)?\(\s*)' + re.escape(lit))
            if not rx.search(line):
                raise SystemExit(f'{rel}:{it["line"]}: literal no encontrado: {lit}')
            line = rx.sub(lambda m: m.group(1) + f"AiStr('{key}')", line, count=1)
        lines[i] = line
    text = '\n'.join(lines)
    # uses de implementación
    if not re.search(r'\buMakerAi\.Localization\b', text):
        ls = text.split('\n')
        impl = next(i for i, l in enumerate(ls) if l.strip().lower() == 'implementation')
        j = impl + 1
        while j < len(ls) and (ls[j].strip() == '' or ls[j].strip().startswith('{$') or ls[j].strip().startswith('//')):
            j += 1
        cr = '\r' if ls[impl].endswith('\r') else ''
        if ls[j].strip().lower() == 'uses':
            ls.insert(j + 1, '  uMakerAi.Localization,' + cr)
        elif ls[j].strip().lower().startswith('uses '):
            ls[j] = re.sub(r'(?i)^(\s*uses\s+)', r'\1uMakerAi.Localization, ', ls[j], count=1)
        else:
            ls[impl + 1:impl + 1] = [cr, 'uses' + cr, '  uMakerAi.Localization;' + cr]
        text = '\n'.join(ls)
    changed_files.append(rel)
    texts[rel] = (text, bom)
    if not dry:
        open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + text.encode('utf-8'))

# Casos manuales
for rel, old, new in getattr(mp, 'PATCHES', []):
    path = os.path.join(SRC, rel)
    if rel in texts:
        text, bom = texts[rel]
    else:
        raw = open(path, 'rb').read()
        bom = raw.startswith(b'\xef\xbb\xbf')
        text = raw.decode('utf-8-sig')
    if '\r\n' in text:
        old, new = old.replace('\n', '\r\n'), new.replace('\n', '\r\n')
    if text.count(old) != 1:
        raise SystemExit(f'{rel}: patch manual con {text.count(old)} coincidencias: {old[:80]}')
    text = text.replace(old, new)
    texts[rel] = (text, bom)
    for k in re.findall(r"AiStr\('([^']+)'\)", new):
        used_keys.add(k)
    if not re.search(r'\buMakerAi\.Localization\b', text):
        raise SystemExit(f'{rel}: patch manual en una unidad sin uses de Localization')
    if not dry:
        open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + text.encode('utf-8'))

# Tablas
def pas(s):
    return "'" + s.replace("'", "''") + "'"

for lang, idx in (('En', 0), ('Es', 1)):
    path = os.path.join(SRC, 'Core', f'uMakerAi.Lang.{lang}.pas')
    text = open(path, 'rb').read().decode('utf-8-sig')
    existing = set(re.findall(r"^\s*'([a-z0-9_.]+)',", text, re.M))
    new_keys = sorted(k for k in used_keys if k not in existing)
    if not new_keys:
        continue
    width = max(len(k) for k in new_keys) + 3
    block = [f'\r\n    // --- {title} ---']
    for k in new_keys:
        block.append(f"    {pas(k) + ',':<{width}} {pas(DEFS[k][idx])},")
    block[-1] = block[-1].rstrip(',')
    pos = text.rindex('\r\n  ];')
    # coma al último par existente
    before = text[:pos].rstrip()
    text = before + ',\r\n' + '\r\n'.join(block) + text[pos:]
    if not dry:
        open(path, 'wb').write(b'\xef\xbb\xbf' + text.encode('utf-8'))

print('archivos:', len(changed_files), '| claves usadas:', len(used_keys))
