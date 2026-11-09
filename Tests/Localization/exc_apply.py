"""Aplica un mapa de excepciones: literal -> AiStr(clave), uses y tablas.

Uso: python exc_apply.py items.json map_module "Título de la sección"
     [--dry]

El mapa trae, por literal, (clave, inglés, español, portugués): un texto por
cada tabla uMakerAi.Lang.*.pas que exista, en el orden En, Es, Pt.
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
# Idiomas con tabla, en el orden de los textos del mapa: (clave, en, es, pt)
LANGS = [l for l in ('En', 'Es', 'Pt')
         if os.path.exists(os.path.join(SRC, 'Core', f'uMakerAi.Lang.{l}.pas'))]
DEFS = {}
for k, v in list(mp.M.items()) + [(None, v) for v in mp.MANUAL.values()]:
    key, texts = v[0], tuple(v[1:])
    if texts[0] is not None:
        if len(texts) < len(LANGS):
            raise SystemExit(f'clave {key}: faltan textos; el mapa debe traer {", ".join(LANGS)}')
        if key in DEFS and DEFS[key] != texts:
            raise SystemExit(f'clave {key} con dos textos distintos')
        DEFS[key] = texts
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

def add_uses(text, rel):
    """Agrega uMakerAi.Localization al uses de implementation si no está."""
    if re.search(r'\buMakerAi\.Localization\b', text):
        return text
    ls = text.split('\n')
    impl = next(i for i, l in enumerate(ls) if l.strip().lower() == 'implementation')
    j = impl + 1
    # Se saltan blancos, comentarios y directivas que no abren una rama
    # ({$R *.dfm}, {$WARN ...}); un {$IF...} se revisa abajo
    while j < len(ls) and (ls[j].strip() == '' or ls[j].strip().startswith('//') or
                           (ls[j].strip().startswith('{$') and not ls[j].strip().upper().startswith('{$IF'))):
        j += 1
    if ls[j].strip().upper().startswith('{$IF'):
        # ¿El uses está bajo la directiva? Un uses condicional no se puede
        # ampliar sin romper la otra rama: hay que reescribirlo a mano como un
        # solo uses incondicional con la parte condicional adentro. Si lo que
        # sigue a la directiva es código, se agrega un uses propio antes.
        k = j + 1
        while k < len(ls) and (ls[k].strip() == '' or ls[k].strip().startswith('{$') or ls[k].strip().startswith('//')):
            k += 1
        if k < len(ls) and re.match(r'(?i)uses\b', ls[k].strip()):
            raise SystemExit(f'{rel}: el uses de implementation está bajo una directiva; ajustarlo a mano')
        cr = '\r' if ls[impl].endswith('\r') else ''
        ls[impl + 1:impl + 1] = [cr, 'uses' + cr, '  uMakerAi.Localization;' + cr]
        return '\n'.join(ls)
    cr = '\r' if ls[impl].endswith('\r') else ''
    if ls[j].strip().lower() == 'uses':
        ls.insert(j + 1, '  uMakerAi.Localization,' + cr)
    elif ls[j].strip().lower().startswith('uses '):
        ls[j] = re.sub(r'(?i)^(\s*uses\s+)', r'\1uMakerAi.Localization, ', ls[j], count=1)
    else:
        ls[impl + 1:impl + 1] = [cr, 'uses' + cr, '  uMakerAi.Localization;' + cr]
    return '\n'.join(ls)


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
            # 'texto: ' + expr  ->  CreateFmt(AiStr(clave), [expr]); con o sin ';'
            # al final (antes de un else no lo lleva)
            rx = re.compile(r'\.Create\(\s*' + re.escape(lit) + r'\s*\+\s*(.*)\)(;?)(\s*)$')
            m = rx.search(line)
            if not m or m.group(1).lstrip().startswith("'"):
                # 'texto' + 'más texto': el mensaje está partido, va a mano
                raise SystemExit(f'{rel}:{it["line"]}: concatenación no reconocida: {line.strip()}')
            line = line[:m.start()] + f".CreateFmt(AiStr('{key}'), [{m.group(1)}]){m.group(2)}" + m.group(3)
        else:
            rx = re.compile(r'(\.Create(?:Fmt)?\(\s*)' + re.escape(lit))
            m0 = rx.search(line)
            if not m0:
                raise SystemExit(f'{rel}:{it["line"]}: literal no encontrado: {lit}')
            if line[m0.end():].lstrip().startswith('#'):
                # 'texto'#13#10'%s': el literal sigue con caracteres pegados
                raise SystemExit(f'{rel}:{it["line"]}: literal con #nn pegado; ajustarlo a mano')
            line = rx.sub(lambda m: m.group(1) + f"AiStr('{key}')", line, count=1)
        lines[i] = line
    text = add_uses('\n'.join(lines), rel)
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
    text = add_uses(text.replace(old, new), rel)
    texts[rel] = (text, bom)
    for k in re.findall(r"AiStr\('([^']+)'\)", new):
        used_keys.add(k)
    if not dry:
        open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + text.encode('utf-8'))

# Tablas
def pas(s):
    return "'" + s.replace("'", "''") + "'"

for idx, lang in enumerate(LANGS):
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
