"""Mensajes de error que llegan a OnError: literal -> AiStr/AiFmt(clave).

Complementa a exc_extract.py/exc_apply.py (que tratan los raise). Aquí el
mensaje es el primer argumento de DoError / ReportError / HandleError /
HandleThreadError / FOnError(Self, ...), escrito como literal, como
concatenación ('texto: ' + E.Message) o como Format('texto %d', [...]).

  1. python err_apply.py extract errores.json
     -> por cada llamada: archivo, línea, plantilla ('texto: %s') y argumentos
  2. escribir map_errores.py con M = {plantilla: (clave, inglés, español, portugués)}
     (las traducciones conservan los marcadores %s/%d en el mismo orden)
  3. python err_apply.py apply errores.json map_errores "Título" [--dry]
  4. hardcoded_strings.py --check-keys y --strict, compilar, correr la suite

El primer argumento se reemplaza por AiStr('clave') si no lleva argumentos o
por AiFmt('clave', [args]); el resto de la llamada queda igual.
"""
import importlib, json, os, re, sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from hardcoded_strings import RE_ERR, strip_comments, is_intentional

SRC = os.path.join(os.path.dirname(os.path.dirname(HERE)), 'Source')
RE_CALL = re.compile(r"\b(?:DoError|ReportError|HandleError|HandleThreadError|FOnError)\s*\(\s*(?:Self\s*,\s*)?")


def read(path):
    raw = open(path, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    return raw.decode('utf-8-sig'), bom


def tokens(s, i):
    """Recorre el primer argumento desde i: devuelve (fin, piezas de nivel 0)."""
    depth, start, pieces, j = 0, i, [], i
    while j < len(s):
        c = s[j]
        if c == "'":
            k = j + 1
            while k < len(s):
                if s[k] == "'":
                    if k + 1 < len(s) and s[k + 1] == "'":
                        k += 2
                        continue
                    break
                k += 1
            j = k + 1
            continue
        if c in '([':
            depth += 1
        elif c in ')]':
            if depth == 0:
                break
            depth -= 1
        elif c == ',' and depth == 0:
            break
        elif c == '+' and depth == 0:
            pieces.append(s[start:j].strip())
            start = j + 1
        j += 1
    pieces.append(s[start:j].strip())
    return j, pieces


def is_lit(p):
    return re.fullmatch(r"'(?:[^']|'')*'", p) is not None


def unq(p):
    return p[1:-1].replace("''", "'")


def analyze(s, a, b, pieces):
    """Plantilla y argumentos del primer argumento s[a:b]."""
    arg = s[a:b].strip()
    m = re.match(r"Format\(\s*('(?:[^']|'')*')\s*,\s*\[", arg, re.S)
    if m and len(pieces) == 1:
        inner = arg[m.end():]
        k = inner.rindex(']')
        return unq(m.group(1)), inner[:k].strip(), 'fmt'
    tpl, args = '', []
    for p in pieces:
        if is_lit(p):
            tpl += unq(p).replace('%', '%%')
        else:
            tpl += '%s'
            args.append(' '.join(p.split()))
    return tpl, ', '.join(args), 'concat'


def extract(out):
    items = []
    for dp, _, fs in os.walk(SRC):
        for f in sorted(fs):
            if not f.endswith('.pas') or f.startswith('uMakerAi.Lang.'):
                continue
            p = os.path.join(dp, f)
            rel = os.path.relpath(p, SRC).replace('\\', '/')
            text, _ = read(p)
            clean = strip_comments(text.replace('\r\n', '\n'))
            for m in RE_CALL.finditer(clean):
                # solo si el primer argumento empieza con literal o Format('...
                rest = clean[m.end():]
                if not re.match(r"(?:Format\(\s*)?'", rest):
                    continue
                lm = RE_ERR.search(clean, m.start())
                if not lm or lm.start() != m.start() or not re.search(r'[^\W\d_]{2}', lm.group(1)):
                    continue
                if is_intentional(rel, lm.group(1)):
                    continue
                end, pieces = tokens(clean, m.end())
                tpl, args, kind = analyze(clean, m.end(), end, pieces)
                items.append({'file': rel, 'line': clean.count('\n', 0, m.start()) + 1,
                              'orig': clean[m.end():end], 'tpl': tpl, 'args': args, 'kind': kind})
    json.dump(items, open(out, 'w', encoding='utf-8'), ensure_ascii=False, indent=1)
    print(len(items), 'llamadas,', len({i['tpl'] for i in items}), 'plantillas distintas')


def apply(items_path, map_mod, title, dry):
    sys.path.insert(0, os.getcwd())
    mp = importlib.import_module(map_mod)
    from exc_apply_lib import add_uses, write_tables
    items = json.load(open(items_path, encoding='utf-8'))
    missing = sorted({i['tpl'] for i in items if i['tpl'] not in mp.M})
    if missing:
        print('SIN MAPA:')
        for t in missing:
            print('  ', repr(t))
        raise SystemExit(1)
    defs, used = {}, set()
    for tpl, v in mp.M.items():
        key, texts = v[0], tuple(v[1:])
        if key in defs and defs[key] != texts:
            raise SystemExit(f'clave {key} con dos textos distintos')
        defs[key] = texts
        marks = re.findall(r'%[sd]', texts[0].replace('%%', ''))
        for t in texts[1:]:
            if re.findall(r'%[sd]', t.replace('%%', '')) != marks:
                raise SystemExit(f'clave {key}: marcadores distintos entre idiomas')
    by_file = {}
    for it in items:
        by_file.setdefault(it['file'], []).append(it)
    for rel, its in sorted(by_file.items()):
        path = os.path.join(SRC, rel)
        text, bom = read(path)
        crlf = '\r\n' in text
        t = text.replace('\r\n', '\n')
        # de abajo hacia arriba: un reemplazo de varias líneas no mueve las siguientes
        for it in sorted(its, key=lambda i: -i['line']):
            key = mp.M[it['tpl']][0]
            used.add(key)
            new = f"AiFmt('{key}', [{it['args']}])" if it['args'] else f"AiStr('{key}')"
            if t.count(it['orig']) < 1:
                raise SystemExit(f'{rel}:{it["line"]}: no se encontró {it["orig"][:60]}')
            # reemplaza la primera aparición que siga a la llamada en esa línea
            lines = t.split('\n')
            off = sum(len(l) + 1 for l in lines[:it['line'] - 1])
            k = t.find(it['orig'], off)
            if k < 0:
                raise SystemExit(f'{rel}:{it["line"]}: argumento fuera de lugar')
            t = t[:k] + new + t[k + len(it['orig']):]
        t = add_uses(t, rel)
        if crlf:
            t = t.replace('\n', '\r\n')
        if not dry:
            open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + t.encode('utf-8'))
    write_tables(SRC, used, defs, title, dry)
    print('archivos:', len(by_file), '| claves usadas:', len(used))


if __name__ == '__main__':
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(encoding='utf-8', errors='replace')
    if sys.argv[1] == 'extract':
        extract(sys.argv[2])
    else:
        apply(sys.argv[2], sys.argv[3], sys.argv[4], '--dry' in sys.argv)
